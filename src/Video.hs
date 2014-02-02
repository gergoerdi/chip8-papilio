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
    let (_, buttonUp', _) = debounce (Witness :: Witness X16) buttonUp
    let (_, buttonDown', _) = debounce (Witness :: Witness X16) buttonDown

    let counter = runRTL $ do
            counter <- newReg (0 :: U4)
            WHEN buttonUp' $ counter := reg counter + 1
            WHEN buttonDown' $ counter := reg counter - 1
            return $ reg counter

    leds (fromUnsigned counter)

emitBench :: IO ()
emitBench = do
    kleg <- reifyFabric $ do
        theClk "CLK_50MHZ"
        testBench

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "Video"
    outPath = ".." </> "ise" </> "gensrc"
    outVHDL name = outPath </> name <.> "vhdl"

main :: IO ()
main = do
    emitBench
