{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Video (main, emitBench, vgaFB, encodeVGA) where

import Types
import Utils
import DCM

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA
import Hardware.KansasLava.VGA.Driver

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits

import System.FilePath
import System.Directory

drive64x32 :: (Clock clk)
           => VGADriverIn clk Bool () ()
           -> (Signal clk Bool, VGADriverOut clk (W VidX) (W VidY) U4 U4 U4)
drive64x32 VGADriverIn{..} = (newPixel,
                              VGADriverOut{ vgaOutX = x'
                                          , vgaOutY = y'
                                          , ..})
  where
    VGADriverOut{..} = driveVGA vga640x480at60 (VGADriverIn r g b)

    (validX, x) = unpackEnabled vgaOutX
    (validY, y) = unpackEnabled vgaOutY

    inFieldH = validX .&&. x `betweenCO` (64, 576)
    inFieldV = validY .&&. y `betweenCO` (112, 368)
    inField = inFieldH .&&. inFieldV

    x' = mapEnabled (\x -> signed $ (x - 64) `shiftR` 3) vgaOutX
    y' = mapEnabled (\y -> signed $ (y - 112) `shiftR` 3) vgaOutY

    newPixel = bitNot vgaOutClkPhase .&&. inField .&&. ((x - 64) .&. 0x7 .==. 0)

    pixel = vgaInR
    r = mux inField (maxBound, spread pixel)
    g = mux inField (minBound, spread pixel)
    b = mux inField (minBound, spread pixel)

    spread s = mux s (minBound, maxBound)

vgaFB :: forall clk. (Clock clk)
      => Signal clk PixData
      -> (Signal clk (Enabled (VidX, VidY)), VGADriverOut clk (W VidX) (W VidY) U4 U4 U4)
vgaFB pixel = (packEnabled newPixel pos, vga)
  where
    (newPixel, vga@VGADriverOut{..}) = drive64x32 VGADriverIn{..}

    vgaInR = pixel'
    vgaInG = pureS ()
    vgaInB = pureS ()

    x = let (en, v) = unpackEnabled vgaOutX in mux en (0, v)
    y = let (en, v) = unpackEnabled vgaOutY in mux en (0, v)

    pos :: Signal clk (VidX, VidY)
    pos = pack (x, y)

    pixel' = runRTL $ do
        prevNewPixel <- newReg False
        prevNewPixel := newPixel

        p <- newReg False
        WHEN (reg prevNewPixel) $ p := pixel
        return (var p)

testBench :: (Arcade fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
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
        (fb, _) = ramWithInit nextPair (rom `flip` initImage) writeFB
        (fbA, vgaS) = vgaFB fbPixel

        fbNextPixel = packEnabled (isEnabled fbA) $
                      mux (isEnabled fbA) (low, syncRead fb $ enabledVal fbA)

        fbPixel = registerEnabled False fbNextPixel

    vga . encodeVGA . vgaOut $ vgaS
  where
    initImage (x, y) = Just $ x `elem` [minBound, maxBound]
                            || y `elem` [minBound, maxBound]

emitBench :: String -> Fabric () -> IO ()
emitBench modName bench = do
    kleg <- reifyFabric $ do
        theClk clock
        wing_init
        bench

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    mod <- netlistCircuit modName kleg
    let mod' = dcm50MHz clock mod

    writeFile (outVHDL modName) (genVHDL mod' ["work.lava.all", "work.all"])
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    clock = "CLK_50MHZ"

    outPath = ".." </> "ise" </> "gensrc"
    outVHDL name = outPath </> name <.> "vhdl"

main :: IO ()
main = do
    emitBench "Video" testBench
