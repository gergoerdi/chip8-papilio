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
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

import Data.Sized.Matrix (matrix, Matrix)
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Foldable (toList)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid

linkProg :: ByteString -> ByteString
linkProg = (prelude <>)
  where
    prelude = fontROM <> BS.replicate (0x200 - BS.length fontROM) 0

fontROM :: ByteString
fontROM = BS.pack . concatMap (padr 8 0 . toList) . toList $ fontData

imageROM :: ByteString -> Addr -> Byte
imageROM bs = \i -> let i' = fromIntegral i
                    in if i' < len then fromIntegral (BS.index bs i') else 0
  where
    len = BS.length bs

chip8 :: forall clk. (Clock clk)
      => ByteString
      -> (Signal clk (Enabled (Bool, X16)), Signal clk (Matrix X16 Bool))
      -> (CPUDebug clk, VGA clk U4 U4 U4)
chip8 prog (kevent, kbd) = (cpuDebug, vgaOut)
  where
    (CPUOut{..}, cpuDebug) = cpu CPUIn{..}
    (fb, fillFB) = ramWithInit nextPair (const low) $
                   packEnabled (enA .&&. enW) $ pack (a, w)
    -- (fb, fillFB) = (writeMemory $ packEnabled (enW .&&. enA) (pack (a, w)), low)
      where
        (enW, w) = unpackEnabled cpuFBW
        (enA, a) = unpackEnabled cpuFBA

    (ram, fillRAM) = ramWithInit (+1) initROM cpuMemW

    cpuMemD = syncRead ram cpuMemA
    cpuStart = bitNot fillRAM .&&. bitNot fillFB
    cpuVBlank = vgaOutVBlank
    cpuKeyEvent = kevent
    cpuKeys = eventLatchRelease inputTimer cpuClearKey kbd
      where
        inputTimer = runRTL $ do
            r <- newReg (0 :: U2)
            WHEN vgaOutVBlank $ r := reg r + 1
            return $ vgaOutVBlank .&&. (reg r .==. 0)

    initROM :: Signal clk Addr -> Signal clk Byte
    initROM = flip rom $ Just . imageROM (linkProg prog)

    (vgaD, cpuFBD) = syncReadBus fb (vgaPos, cpuFBA)
    (vgaPos, VGADriverOut{..}) = vgaFB vgaD

main :: IO ()
main = do
    -- [filename] <- getArgs
    -- let filename = "/home/cactus/prog/haskell/chip8/games/2048/2048.ch8"
    let filename = "/home/cactus/prog/haskell/chip8/import/CHIP8/GAMES/TETRIS"
    prog <- BS.readFile filename
    emitBench "Chip8" $ do
        kbd <- chip8Keyboard
        let (CPUDebug{..}, vgaOut) = chip8 prog kbd
        vga . encodeVGA $ vgaOut
        leds $ matrix [ cpuState .==. pureS Halt
                      , cpuState .==. pureS ClearFB
                      , cpuState .==. pureS WaitKey
                      , cpuState .==. pureS Draw
                      ]
