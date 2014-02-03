{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
import Utils

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA
import VGA640x480

import Data.Sized.Matrix (Matrix, matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Control.Monad (liftM)

import System.FilePath
import System.Directory

video :: VideoIn -> VideoOut
video VideoIn{..} = runRTL $ do
    fbAddr <- newReg (0, 0)
    let vidFrameBufferA = reg fbAddr
    return VideoOut{..}
-}

vgaFB :: Signal CLK Bool
      -> Signal CLK Bool -> Signal CLK Bool -> Signal CLK Bool
      -> VGA CLK X4 X4 X4
vgaFB reset r g b = vgaOut
  where
    VGADriverOut{..} = driveVGA VGADriverIn{..}

    vgaInReset = reset
    vgaInR = mux inField (pureS 0, binarize r)
    vgaInG = mux inField (pureS 0, binarize g)
    vgaInB = mux inField (pureS 0, binarize b)

    inField = isEnabled vgaOutY .&&.
              enabledVal vgaOutY `betweenCO` (80, 400)

    binarize s = mux s (pureS minBound, pureS maxBound)

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    (_, reset, _) <- debounce (Witness :: Witness X16) `liftM` resetButton
    let (_, buttonR, _) = debounce (Witness :: Witness X16) buttonLeft
    let (_, buttonG, _) = debounce (Witness :: Witness X16) buttonUp
    let (_, buttonB, _) = debounce (Witness :: Witness X16) buttonRight

    let r = toggle buttonR
        g = toggle buttonG
        b = toggle buttonB

    leds $ matrix [reset, r, g, b]
    vga . encodeVGA $ vgaFB reset r g b
  where
    toggle btn = runRTL $ do
        buf <- newReg False
        WHEN btn $ buf := bitNot (reg buf)
        return $ reg buf

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
