{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
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

nextPair xy = mux (x .==. maxBound) (pack (x + 1, y), pack (0, y + 1))
  where
    (x, y) = unpack xy
    nextRow = x .==. maxBound

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

    x = signed $ (rawX - 64) `shiftR` 3
    y = signed $ (rawY - 112) `shiftR` 3

    pos :: Signal clk (VidX, VidY)
    pos = pack (x, y)

    pixel = syncRead fb (nextPair pos)

    -- r = mux (x .==. 0) (spread pixel, 0)
    -- g = mux (x .==. 1) (spread pixel, 0)
    r = spread pixel
    g = spread pixel
    b = spread pixel

    spread s = mux s (minBound, maxBound)

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    (_, reset, _) <- debounce (Witness :: Witness X16) `liftM` resetButton

    let (_, buttonUp', _) = debounce (Witness :: Witness X16) buttonUp
    let (_, buttonDown', _) = debounce (Witness :: Witness X16) buttonDown
    let (_, buttonLeft', _) = debounce (Witness :: Witness X16) buttonLeft
    let (_, buttonRight', _) = debounce (Witness :: Witness X16) buttonRight

    let (we, cursor) = runRTL $ do
            x <- newReg 0
            y <- newReg 0

            WHEN buttonUp' $ y := reg y - 1
            WHEN buttonDown' $ y := reg y + 1
            WHEN buttonLeft' $ x := reg x - 1
            WHEN buttonRight' $ x := reg x + 1

            let anyButton = buttonUp' .||. buttonDown' .||. buttonLeft' .||. buttonRight'
            return (anyButton, pack (reg x, reg y))

    let fb = ramWithInit nextPair (rom `flip` initImage) $ packEnabled we $ pack (cursor, d)
        d = bitNot $ syncRead fb cursor

    vga . encodeVGA $ vgaFB reset fb
  where
    toggle btn = runRTL $ do
        buf <- newReg False
        WHEN btn $ buf := bitNot (reg buf)
        return $ reg buf

    initImage (x, y) = Just $ x `elem` [minBound, maxBound] || y `elem` [minBound, maxBound] || fromIntegral x == fromIntegral y

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
