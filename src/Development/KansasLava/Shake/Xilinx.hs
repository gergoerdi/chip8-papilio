{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Development.KansasLava.Shake.Xilinx
       ( XilinxConfig(..)
       , xilinxRules
       ) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

import Data.Monoid
import Data.List (stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map

data XilinxConfig = XilinxConfig{ xilinxRoot :: FilePath
                                , xilinxPlatform :: String
                                }

xilinxRules :: XilinxConfig
            -> String
            -> [String]
            -> Rules ()
xilinxRules XilinxConfig{..} mod xaws = do
    "*.ut" *>
        textTemplate []

    "*.xst" *>
        textTemplate [("MAIN", fromString mod), ("TOP", fromString mod)]

    "*.xise" *>
        textTemplate [("FILES", xiseFiles)]

    "*.prj" *> \target -> do
        let vhdlWork baseName = mconcat ["vhdl work \"", baseName <.> "vhdl", "\""]
        liftIO $ writeFile target . unlines $
          map (vhdlWork . gensrc) vhdls ++ map (vhdlWork . xawsrc) xaws

    xawsrc "*.vhdl" *> \target -> do
        let xaw = ".." </> "xaw" </> takeFileName target -<.> "xaw"
        need [xaw]
        removeFilesAfter "." ["xaw2vhdl.log"]
        xilinx "xaw2vhdl" [xaw, "-st", "XST", target]
    xawsrc "*.ucf" *> \target -> need [target -<.> "vhdl"]

    "xst/projnav.tmp" *> liftIO . createDirectoryIfMissing True

    "*.ngc" *> \target -> do
        need $
          (target -<.> "prj"):
          (target -<.> "xst"):
          ("xst" </> "projnav.tmp"):
          [gensrc $ f <.> "vhdl" | f <- vhdls] ++
          [xawsrc $ f <.> "vhdl" | f <- xaws]

        removeFilesAfter "."
          [ "xst//*"
          , "_xmsgs//*"
          , target -<.> "lso"
          , target -<.> "ngr"
          , target -<.> "syr"
          , "*.xrpt"
          ]
        xilinx "xst" [ "-ifn", target -<.> "xst"
                     , "-ofn", target -<.> "syr"
                     ]

    "*.ngd" *> \target -> do
        let ucf = gensrc $ target -<.> "ucf"
        need [ target -<.> "ngc"
             , ucf
             , "xst/projnav.tmp"
             ]
        removeFilesAfter "."
          [ target -<.> "bld"
          , "ngo//*"
          , "xlnx_auto_0_xdb//*"
          , "*.xrpt"
          ]
        xilinx "ngdbuild" [ "-dd", "ngo"
                          , "-nt", "timestamp"
                          , "-uc", ucf
                          , "-p", xilinxPlatform
                          , target -<.> "ngc"
                          , target
                          ]

    "*.pcf" *> \target -> do
        need [ target -<.> "ngc"
             , target -<.> "ngd"
             , "xst/projnav.tmp"
             ]
        removeFilesAfter "."
          [ "*_summary.xml"
          , "*_usage.xml"
          , target -<.> "ngm"
          , target -<.> "mrp"
          , target -<.> "map"
          ]
        xilinx "map" [ "-p", xilinxPlatform
                     , "-cm", "area"
                     , "-ir", "off"
                     , "-pr", "off"
                     , "-c", "100"
                     , "-o", mapFileName (<> "_map") target -<.> "ncd"
                     , target -<.> "ngd"
                     , target -<.> "pcf"
                     ]

    alternatives $ do
        "*_map.ncd" *> \target -> need [mapFileName (fromJust . stripSuffix "_map") target -<.> "pcf"]

        "*.ncd" *> \target -> do
            need [ "xst" </> "projnav.tmp"
                 , target -<.> "pcf"
                 ]
            removeFilesAfter "."
              [ "*_pad.txt"
              , "*_pad.xrpt"
              , "*_pad.csv"
              , "_xmsgs//*"
              , target -<.> "pad"
              , target -<.> "par"
              , target -<.> "xpi"
              , target -<.> "unroutes"
              , target -<.> "ptwx"
              ]
            xilinx "par" [ "-w"
                         , "-ol", "high"
                         , "-t", "1"
                         , mapFileName (<> "_map") target -<.> "ncd"
                         , target -<.> "ncd"
                         , target -<.> "pcf"
                         ]

    "*.bit" *> \target -> do
        need [ "xst/projnav.tmp"
             , target -<.> "ut"
             , target -<.> "ncd"
             ]
        removeFilesAfter "."
          [ "*_bitgen.xwbt"
          , "usage_statistics_webtalk.html"
          , "webtalk.log"
          , target -<.> "bgn"
          , target -<.> "drc"
          ]
        xilinx "bitgen" [ "-f", target -<.> "ut"
                        , target -<.> "ncd"
                        ]
  where
    xilinx tool args = cmd (xilinxRoot </> tool) args

    gensrc f = "gensrc" </> f
    xawsrc f = gensrc $ "xaw" </> f

    templateFile f = ".." </> "ise.template" </> f

    vhdls = [mod, "lava-prelude"]

    textTemplate replacements target = do
        let ext = drop 1 . takeExtension $ target
            fileName = templateFile $ ext <.> "in"
        need [fileName]
        t <- liftIO $ Text.readFile fileName
        writeFileChanged target $ Text.unpack . substituteTemplate replacements $ t

    xiseFiles = Text.unlines $
        [ "<file xil_pn:name=\"" <> fileName <> "\" xil_pn:type=\"FILE_VHDL\">\n" <>
          "  <association xil_pn:name=\"BehavioralSimulation\"/>\n" <>
          "  <association xil_pn:name=\"Implementation\"/>\n" <>
          "</file>"
        | vhdl <- vhdls
        , let fileName = fromString $ vhdl <.> "vhdl"
        ] ++
        [ "<file xil_pn:name=\"" <> fromString (mod <.> "ucf") <> "\" xil_pn:type=\"FILE_UCF\">\n" <>
          "  <association xil_pn:name=\"Implementation\"/>\n" <>
          "</file>"
        ] ++
        [ "<file xil_pn:name=\"" <> fileName <> "\" xil_pn:type=\"FILE_XAW\">\n" <>
          "  <association xil_pn:name=\"BehavioralSimulation\"/>\n" <>
          "  <association xil_pn:name=\"Implementation\"/>\n" <>
          "</file>"
        | xaw <- xaws
        , let fileName = fromString $ ".." </> "xaw" </> xaw <.> "xaw"
        ]

mapFileName :: (String -> String) -> FilePath -> FilePath
mapFileName f fp = replaceFileName fp (f (takeFileName fp))

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

substituteTemplate :: [(Text, Text)] -> Text -> Text
substituteTemplate replacements = mconcat . go . Text.splitOn "@@"
  where
    subs :: Map Text Text
    subs = Map.fromList replacements

    go :: [Text] -> [Text]
    go (pre:key:ts) = pre : fromMaybe key (Map.lookup key subs) : go ts
    go ts = ts
