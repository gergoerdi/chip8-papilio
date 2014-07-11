{-# LANGUAGE OverloadedStrings #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

import Heist
import Heist.Interpreted

import qualified Text.XmlHtml as X

import Control.Monad.Identity
import Control.Monad.Trans.Either
import Data.Monoid
import Data.String
import Data.List (stripPrefix)
import Data.Maybe (fromJust)

import Blaze.ByteString.Builder
import qualified Data.ByteString as BS

main :: IO ()
main = do
    heist <- do
        heist <- runEitherT $ initHeist heistConfig
        hs <- case heist of
            Left errs -> error $ unlines ("Initializing Heist failed:":errs)
            Right hs -> return hs
        return $ \templateName splices -> case runIdentity $ renderTemplate (bindSplices splices hs) templateName of
            Nothing -> error "renderTemplate failed"
            Just (builder, _mimeType) -> toByteString builder

    setCurrentDirectory "ise"

    shakeArgs shakeOptions $ do
        want [ mod <.> "bit" ]

        let textTemplate ext replacements = do
                ("*" <.> ext) *> \target -> do
                    let fileName = templateFile $ ext <.> "in"
                    need [fileName]
                    let subs = [ "s/@" <> binder <> "@/" <> binding <> "/g;"
                               | (binder, binding) <- replacements
                               ]
                    let sed = mconcat subs <> "w" <> target
                    cmd ("sed -n -e" :: String) [sed, fileName]

            heistTemplate ext splices = do
                ("*" <.> ext) *> \target -> do
                    liftIO $ BS.writeFile target $ heist (fromString $ ext) splices

        textTemplate "ut" []
        textTemplate "xst" [("MAIN", mod), ("TOP", top)]
        -- heistTemplate "xst" $ do
        --   "lava-main" ## textSplice (fromString mod)
        --   "lava-top" ## textSplice (fromString top)
        heistTemplate "xise" $
          "lava-files" ## return (concat [ concatMap xiseVHDL vhdls
                                         , concatMap xiseXAW xaws
                                         ])

        "*.prj" *> \target -> do
            let vhdlWork baseName = mconcat ["vhdl work \"", baseName <.> "vhdl", "\""]
            liftIO $ writeFile target . unlines $
              map (vhdlWork . gensrc) vhdls ++ map (vhdlWork . xawsrc) xaws

        xawsrc "*.vhdl" *> \target -> do
            let xaw = ipcore $ takeBaseName target <.> "xaw"
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
            let ucf = gensrc $ mod <.> "ucf"
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
                              , "-p", fpga
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
            xilinx "map" [ "-p", fpga
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
    xilinxRoot = "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
    xilinx tool args = cmd (xilinxRoot </> tool) args

    fpga = "XC3S500E-VQ100-5"

    ipcore f = "ipcore_dir" </> f
    gensrc f = "gensrc" </> f
    xawsrc f = gensrc $ "xaw" </> f

    templateFile f = ".." </> "ise.template" </> f

    mod = "Chip8"
    vhdls = [mod, "lava-prelude"]
    top = "Chip8"
    xaws = ["dcm_32_to_50p35"]

    heistConfig = mempty{ hcTemplateLocations = [loadTemplates "ise.template"] }

    xiseVHDL mod = [ X.Element "file" [("xil_pn:name", fromString $ mod <.> "vhdl")]
                     [ X.Element "association" [("xil_pn:name", "BehavioralSimulation")] []
                     , X.Element "association" [("xil_pn:name", "Implementation")] []
                     ]
                   ]
    xiseXAW mod =  [ X.Element "file" [ ("xil_pn:name", fromString $ mod <.> "xaw")
                                      , ("xil_pn:type", "FILE_XAW")
                                      ]
                     [ X.Element "association" [("xil_pn:name", "BehavioralSimulation")] []
                     , X.Element "association" [("xil_pn:name", "Implementation")] []
                     ]
                  ]

mapFileName :: (String -> String) -> FilePath -> FilePath
mapFileName f fp = replaceFileName fp (f (takeFileName fp))

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse
