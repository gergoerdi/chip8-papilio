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

    shakeArgs shakeOptions $ do
        want [ out $ mod <.> "bit" ]
        want [ out $ xawsrc $ xaw <.> "vhdl" | xaw <- xaws ]

        let textTemplate ext replacements = do
                out ("*" <.> ext) *> \target -> do
                    let fileName = templateFile $ ext <.> "in"
                    need [fileName]
                    let subs = [ "s/@" <> binder <> "@/" <> binding <> "/g;"
                               | (binder, binding) <- replacements
                               ]
                    let sed = mconcat subs <> "w" <> target
                    cmd ("sed -n -e" :: String) [sed, fileName]

            heistTemplate ext splices = do
                out ("*" <.> ext) *> \target -> do
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

        out "*.prj" *> \target -> do
            let vhdlWork baseName = mconcat ["vhdl work \"", baseName <.> "vhdl", "\""]
            liftIO $ writeFile target . unlines $
              map (vhdlWork . gensrc) vhdls ++ map (vhdlWork . xawsrc) xaws

        out (xawsrc "*.vhdl") *> \target -> do
            let xaw = ipcore $ takeBaseName target <.> "xaw"
            need [out xaw]
            xilinx "xaw2vhdl" [xaw, "-st", "XST", dropDirectory1 target]
        out (xawsrc "*.ucf") *> \target -> need [target -<.> "vhdl"]

        out ("xst" </> "projnav.tmp") *> liftIO . createDirectoryIfMissing True

        out "*.ngc" *> \target -> do
            need $
              (target -<.> "prj"):
              (target -<.> "xst"):
              (out $ "xst" </> "projnav.tmp"):
              [out . gensrc $ f <.> "vhdl" | f <- vhdls]
            xilinx "xst" [ "-ifn", dropDirectory1 target -<.> "xst"
                         , "-ofn", dropDirectory1 target -<.> "syr"
                         ]

        out "*.ngd" *> \target -> do
            let ucf = gensrc $ mod <.> "ucf"
            need [ target -<.> "ngc", out ucf
                 , out $ "xst" </> "projnav.tmp"
                 ]
            xilinx "ngdbuild" [ "-dd", "ngo"
                              , "-nt", "timestamp"
                              , "-uc", ucf
                              , "-p", fpga
                              , dropDirectory1 target -<.> "ngc"
                              , dropDirectory1 target
                              ]

        out "*.pcf" *> \target -> do
            need [ target -<.> "ngc"
                 , target -<.> "ngd"
                 , out $ "xst" </> "projnav.tmp"
                 ]
            xilinx "map" [ "-p", fpga
                         , "-cm", "area"
                         , "-ir", "off"
                         , "-pr", "off"
                         , "-c", "100"
                         , "-o", dropDirectory1 (mapFileName (<> "_map") target -<.> "ncd")
                         , dropDirectory1 target -<.> "ngd"
                         , dropDirectory1 target -<.> "pcf"
                         ]
        alternatives $ do
            out "*_map.ncd" *> \target -> need [mapFileName (fromJust . stripSuffix "_map") target -<.> "pcf"]

            out "*.ncd" *> \target -> do
                need [ out $ "xst" </> "projnav.tmp"
                     , target -<.> "pcf"
                     ]
                xilinx "par" [ "-w"
                             , "-ol", "high"
                             , "-t", "1"
                             , dropDirectory1 (mapFileName (<> "_map") target) -<.> "ncd"
                             , dropDirectory1 target -<.> "ncd"
                             , dropDirectory1 target -<.> "pcf"
                             ]

        out "*.bit" *> \target -> do
            need [ out $ "xst" </> "projnav.tmp"
                 , target -<.> "ut"
                 , target -<.> "ncd"
                 ]
            xilinx "bitgen" [ "-f", dropDirectory1 target -<.> "ut"
                            , dropDirectory1 target -<.> "ncd"
                            ]
  where
    xilinxRoot = "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
    xilinx tool args = cmd (Cwd "ise2") (xilinxRoot </> tool) args

    fpga = "XC3S500E-VQ100-5"

    out f = "ise2" </> f
    ipcore f = "ipcore_dir" </> f
    gensrc f = "gensrc" </> f
    xawsrc f = gensrc $ "xaw" </> f

    templateFile f = "ise.template" </> f

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
