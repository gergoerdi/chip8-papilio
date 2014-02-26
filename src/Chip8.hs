{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Chip8 where

import Types
import Video hiding (main)
import Input hiding (main)
import CPU
import Utils

import Language.KansasLava
-- import Language.KansasLava.Signal
-- import Language.KansasLava.Signal.Utils
-- import Hardware.KansasLava.PS2
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
-- import Hardware.KansasLava.VGA

import Data.Sized.Matrix (matrix, Matrix)
-- import qualified Data.Sized.Matrix as Matrix
-- import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
-- import Data.Traversable (forM)

-- chip8 :: forall clk. (Clock clk) => Signal clk (Matrix X16 Bool) -> VGA clk U4 U4 U4
chip8 kbd = (ram, fb, CPUIn{..}, CPUOut{..}, vgaOut)
  where
    CPUOut{..} = cpu CPUIn{..}
    (fb, fillFB) = ramWithInit nextPair (const low) $ packEnabled (isEnabled cpuFBW) (pack (cpuFBA, enabledVal cpuFBW))
    (ram, fillRAM) = ramWithInit (+1) initROM $ packEnabled (isEnabled cpuMemW) (pack (cpuMemA, enabledVal cpuMemW))

    cpuMemD = syncRead ram cpuMemA
    cpuFBD = syncRead fb cpuFBA
    cpuStart = bitNot fillRAM .&&. bitNot fillFB
    cpuVBlank = vgaOutVBlank
    cpuKeys = kbd

    -- initROM :: Signal clk Addr -> Signal clk Byte
    initROM :: Signal CLK Addr -> Signal CLK Byte
    initROM = flip rom $ \x -> case x of
        0x000 -> Just 0x42 -- 01000010
        0x001 -> Just 0xa5 -- 10100101
        0x002 -> Just 0x99 -- 10011001
        0x003 -> Just 0x42 -- 01000010
        0x004 -> Just 0x42 -- 01000010
        0x005 -> Just 0x24 -- 00100100
        0x006 -> Just 0x18 -- 00011000
        0x007 -> Just 0x03 -- 00000011
        0x200 -> Just 0xd0
        0x201 -> Just 0x08
        0x202 -> Just 0x00
        0x203 -> Just 0x00
        _ -> Just 0x00
    VGADriverOut{..} = vgaFB fb

noKbd :: Signal CLK (Matrix X16 Bool)
noKbd = pureS $ matrix $ replicate 16 False

-- initTime = 8194
initTime = 8204
(ram, fb, cin, cout, _) = chip8 noKbd

len = 30
test = take len . drop initTime . fromS

-- fbTest = take 10 $ drop initTime $ fromS $ fbs
-- sTest = take 10 $ drop initTime $ fromS $ ss
-- opTest = take 10 $ drop initTime $ fromS $ ops

showFB :: ((VidX, VidY) -> PixData) -> String
showFB f = map (\(x,y) -> if f (x,y) then '*' else ' ') [(x, 0) | x <- [0..7]]


main :: IO ()
main = emitBench "Chip8" $ do
    kbd <- chip8Keyboard
    let (_, _, _cpuIn, _cpuOut, vgaOut) = chip8 kbd
    vga . encodeVGA $ vgaOut
