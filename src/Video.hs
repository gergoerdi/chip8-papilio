{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA

import Data.Sized.Matrix (Matrix, matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix

import System.FilePath
import System.Directory

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    leds $ matrix [buttonUp, buttonDown, buttonLeft, buttonRight]

emitBench :: IO ()
emitBench = do
    kleg <- reifyFabric $ do
        board_init
        testBench

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "Video"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"

main :: IO ()
main = do
    emitBench
