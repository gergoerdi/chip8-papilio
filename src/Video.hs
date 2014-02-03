{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Bits

import System.FilePath
import System.Directory

type VidX = U6
type VidY = U5
type PixData = Bool

type FrameBuffer = (VidX, VidY) -> PixData

vgaFB :: forall clk. (Clock clk)
      => Signal clk Bool
      -> Signal clk FrameBuffer
      -> VGA clk X4 X4 X4
vgaFB reset fb = vgaOut
  where
    VGADriverOut{..} = driveVGA VGADriverIn{..}

    vgaInReset = reset
    vgaInR = mux inField (pureS 0, r)
    vgaInG = mux inField (pureS 0, g)
    vgaInB = mux inField (pureS 0, b)

    (validX, rawX) = unpackEnabled vgaOutX
    (validY, rawY) = unpackEnabled vgaOutY

    inFieldH = validX .&&. rawX `betweenCO` (64, 576)
    inFieldV = validY .&&. rawY `betweenCO` (112, 368)
    inField = inFieldH .&&. inFieldV
    -- inField = validY .&&. rawY `betweenCO` (80, 400)

    pos :: Signal clk (VidX, VidY)
    pos = pack (signed $ (rawX - 64) `shiftR` 3, signed $ (rawY - 112) `shiftR` 3)

    pixel = syncRead fb pos

    r = spread pixel
    g = spread pixel
    b = spread pixel

    spread s = mux s (minBound, maxBound)

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    (_, reset, _) <- debounce (Witness :: Witness X16) `liftM` resetButton

    -- let (_, buttonUp, _) = debounce (Witness :: Witness X16) buttonUp
    -- let (_, buttonDown, _) = debounce (Witness :: Witness X16) buttonDown
    -- let (_, buttonLeft, _) = debounce (Witness :: Witness X16) buttonLeft
    -- let (_, buttonRight, _) = debounce (Witness :: Witness X16) buttonRight

    -- let r = toggle buttonR
    --     g = toggle buttonG
    --     b = toggle buttonB

    leds $ matrix [reset, low, low, low]

    let fb = runRTL $ do
            x <- newReg minBound
            y <- newReg minBound
            filled <- newReg False
            let filling = bitNot $ reg filled

            let fbA = pack (reg x, reg y)

            WHEN filling $ do
                -- x := reg x + 1
                x := 0
                y := reg y + 1
                WHEN (reg y .==. maxBound) $ do
                    filled := high

            return $ writeMemory $ packEnabled filling $ pack (fbA, high)

    vga . encodeVGA $ vgaFB reset fb
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
