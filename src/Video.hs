{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
import Utils

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA
import VGADriver

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

nextPair xy = pack (x + 1, mux nextRow (y, y + 1))
  where
    (x, y) = unpack xy
    nextRow = x .==. maxBound

drive64x32 :: (Clock clk)
           => VGADriverIn clk Bool () () -> VGADriverOut clk X6 X5 U4 U4 U4
drive64x32 VGADriverIn{..} = VGADriverOut{ vgaOut = vgaOut
                                         , vgaOutX = x'
                                         , vgaOutY = y' }
  where
    VGADriverOut{..} = driveVGA vga640x480 vgaDriverIn'
    vgaDriverIn' = VGADriverIn{ vgaInReset = vgaInReset
                              , vgaInR = r'
                              , vgaInG = g'
                              , vgaInB = b'
                              }

    r' = mux inField (pureS 0, r)
    g' = mux inField (pureS 0, g)
    b' = mux inField (pureS 0, b)

    (validX, x) = unpackEnabled vgaOutX
    (validY, y) = unpackEnabled vgaOutY

    inFieldH = validX .&&. x `betweenCO` (64, 576)
    inFieldV = validY .&&. y `betweenCO` (112, 368)
    inField = inFieldH .&&. inFieldV

    x' = mapEnabled (\x -> signed $ (x - 64) `shiftR` 3) vgaOutX
    y' = mapEnabled (\y -> signed $ (y - 112) `shiftR` 3) vgaOutY

    pixel = bitwise vgaInR
    r = spread pixel
    g = spread pixel
    b = spread pixel

    spread s = mux s (minBound, maxBound)

vgaFB :: forall clk. (Clock clk)
      => Signal clk Bool
      -> Signal clk FrameBuffer
      -> VGA clk U4 U4 U4
vgaFB reset fb = vgaOut
  where
    VGADriverOut{..} = drive64x32 VGADriverIn{..}

    vgaInReset = reset
    vgaInR = bitwise pixel
    vgaInG = pureS ()
    vgaInB = pureS ()

    x = enabledVal vgaOutX
    y = enabledVal vgaOutY

    pos :: Signal clk (VidX, VidY)
    pos = pack (x, y)

    pixel = syncRead fb pos

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    (_, reset, _) <- debounce (Witness :: Witness X16) `liftM` resetButton

    let (_, buttonUp', _) = debounce (Witness :: Witness X16) buttonUp
    let (_, buttonDown', _) = debounce (Witness :: Witness X16) buttonDown
    let (_, buttonLeft', _) = debounce (Witness :: Witness X16) buttonLeft
    let (_, buttonRight', _) = debounce (Witness :: Witness X16) buttonRight

    let writeFB = runRTL $ do
            x <- newReg 0
            y <- newReg 0
            let cursor = pack (reg x, reg y)
            let anyButton = buttonUp' .||. buttonDown' .||. buttonLeft' .||. buttonRight'

            fetching <- newReg False
            waiting <- newReg False
            we <- newReg False
            d <- newReg True

            CASE [ IF (reg fetching) $ do
                        fetching := low
                        waiting := high
                 , IF (reg waiting) $ do
                        we := high
                        waiting := low
                        d := bitNot $ syncRead fb cursor
                 , OTHERWISE $ do
                        we := low

                        let dy1 = mux buttonUp' (0, -1)
                            dy2 = mux buttonDown' (0, 1)
                            dy = dy1 + dy2

                            dx1 = mux buttonLeft' (0, -1)
                            dx2 = mux buttonRight' (0, 1)
                            dx = dx1 + dx2

                        x := reg x + dx
                        y := reg y + dy
                        fetching := anyButton
                 ]

            return $ packEnabled (reg we) $ pack (cursor, reg d)
        fb = ramWithInit nextPair (rom `flip` initImage) writeFB

    vga . encodeVGA $ vgaFB reset fb
  where
    toggle btn = runRTL $ do
        buf <- newReg False
        WHEN btn $ buf := bitNot (reg buf)
        return $ reg buf

    initImage (x, y) = Just $ x `elem` [minBound, maxBound]
                            || y `elem` [minBound, maxBound]

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
