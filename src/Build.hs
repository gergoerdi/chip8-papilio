{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Development.KansasLava.Shake.Xilinx
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Console.GetOpt
import System.Exit

import qualified Data.ByteString as BS

import Language.KansasLava.VHDL (writeVhdlPrelude)
import qualified Chip8
import Video (synthesize)

data Flag = ImageFile FilePath
          | XilinxRoot FilePath
          | FPGAModel String

mkXilinxConfig :: [Flag] -> IO XilinxConfig
mkXilinxConfig flags = do
    xilinxRoot <- case [path | XilinxRoot path <- flags] of
        [] -> return "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
        [path] -> return path
        _ -> do
            putStrLn "Conflicting flags: --xilinx"
            exitFailure

    xilinxPlatform <- case [model | FPGAModel model <- flags] of
        [] -> return "XC3S500E-VQ100-5"
        [model] -> return model
        _ -> do
            putStrLn "Conflicting flags: --fpga"
            exitFailure

    return $ XilinxConfig{..}

main :: IO ()
main = do
    createDirectoryIfMissing True "ise"
    setCurrentDirectory "ise"
    shakeArgsWith shakeOptions flags $ \flags targets -> do
        prog <- case [fileName | ImageFile fileName <- flags] of
            [fileName] -> BS.readFile fileName
            _ -> do
                putStrLn "Missing flag: --image"
                exitFailure

        xilinxConfig <- mkXilinxConfig flags

        (vhdl, ucf) <- synthesize modName (Chip8.bench prog)
        return $ Just $ do
            want $ if null targets then [modName <.> "bit"] else targets

            lavaRules modName vhdl ucf
            xilinxRules xilinxConfig modName xaws
  where
    flags = [ Option [] ["image"] (ReqArg (Right . ImageFile) "filename") "CHIP-8 image file"
            , Option [] ["xilinx"] (ReqArg (Right . XilinxRoot) "path") "Path to Xilinx toolchain"
            , Option [] ["fpga"] (ReqArg (Right . FPGAModel) "model") "Target FPGA model"
            ]

    modName = "Chip8"
    xaws = ["dcm_32_to_50p35"]

lavaRules :: FilePath -> String -> String -> Rules ()
lavaRules modName vhdl ucf = do
    gensrc modName <.> "vhdl" *> \target -> writeFileChanged target vhdl
    gensrc modName <.> "ucf" *> \target -> writeFileChanged target ucf
    gensrc "lava-prelude.vhdl" *> liftIO . writeVhdlPrelude
  where
    gensrc f = "gensrc" </> f
