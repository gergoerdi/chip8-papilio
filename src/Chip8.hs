{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Chip8 where

import Types
import Video hiding (main)
import Input hiding (main)
import CPU
import Font
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
import Data.Foldable (toList)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid

linkProg :: ByteString -> ByteString
linkProg = (prelude <>)
  where
    prelude = fontROM <> BS.replicate (0x200 - BS.length fontROM) 0
fontROM = BS.pack . concatMap (padr 8 0 . toList) . toList $ fontData

imageROM :: ByteString -> Addr -> Byte
imageROM bs = \i -> let i' = fromIntegral i
                    in if i' < len then fromIntegral (BS.index bs i') else 0
  where
    len = BS.length bs

-- chip8 :: forall clk. (Clock clk) => Signal clk (Matrix X16 Bool) -> VGA clk U4 U4 U4
chip8 prog (kevent, kbd) = (ram, fb, CPUIn{..}, CPUOut{..}, vgaOut)
  where
    CPUOut{..} = cpu CPUIn{..}
    (fb, fillFB) = ramWithInit nextPair (const low) $
                   packEnabled (isEnabled cpuFBA .&&. isEnabled cpuFBW) $
                   pack (enabledVal cpuFBA, enabledVal cpuFBW)
    (ram, fillRAM) = ramWithInit (+1) initROM cpuMemW

    cpuMemD = syncRead ram cpuMemA
    cpuStart = bitNot fillRAM .&&. bitNot fillFB
    cpuVBlank = vgaOutVBlank
    cpuKeyEvent = kevent
    cpuKeys = kbd

    -- initROM :: Signal clk Addr -> Signal clk Byte
    initROM :: Signal CLK Addr -> Signal CLK Byte
    initROM = flip rom $ Just . imageROM (linkProg prog)

    (vgaD, cpuFBD) = syncReadBus fb (vgaPos, cpuFBA)
    (vgaPos, VGADriverOut{..}) = vgaFB vgaD

noKbd :: Signal CLK (Matrix X16 Bool)
noKbd = pureS $ matrix $ replicate 16 False

testProg :: ByteString
testProg = BS.pack [ 0xf1, 0x0a -- WaitKey V1
                   , 0x00, 0xe0 -- ClrScr
                   , 0xf1, 0x29 -- LoadFont V1
                   , 0xd0, 0x05 -- Draw V0 V0 5
                   , 0x12, 0x00 -- Jmp 0x200
                   ]

keyInput :: Signal CLK (Enabled (Bool, X16))
keyInput = toS . mconcat $
           [ replicate (initTime + 200) Nothing
           , cycle . concat $
             replicate 10 [Just (True, 1), Just (False, 1)] ++ replicate 5 [Nothing]
           ]

initTime = 8192
fullTime = initTime + 1000
(ram, fb, cin, cout, _) = chip8 testProg kbd
  where
    kbd = (keyInput, eventLatch keyInput)

len = 20
test = take len . drop initTime . fromS

-- fbTest = take 10 $ drop initTime $ fromS $ fbs
-- sTest = take 10 $ drop initTime $ fromS $ ss
-- opTest = take 10 $ drop initTime $ fromS $ ops

showFB :: ((VidX, VidY) -> PixData) -> String
showFB f = map (\(x,y) -> if f (x,y) then '*' else ' ') [(x, 0) | x <- [0..7]]


main :: IO ()
main = do
    -- [filename] <- getArgs
    -- let filename = "/tmp/15puzzle.ch8"
    -- prog <- BS.readFile filename
    emitBench "Chip8" $ do
        kbd <- chip8Keyboard
        let (_, _, _cpuIn, cpuOut, vgaOut) = chip8 testProg kbd
        vga . encodeVGA $ vgaOut
        leds $ matrix [ high
                      , cpuState cpuOut .==. pureS ClearFB
                      , cpuState cpuOut .==. pureS WaitKey
                      , cpuState cpuOut .==. pureS Draw
                      ]
