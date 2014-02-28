{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Input (main, chip8Keyboard) where

import Video hiding (main)

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.PS2
import Hardware.KansasLava.Boards.Papilio.Arcade

import Data.Sized.Matrix (matrix, Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Traversable (forM)

-- TODO: This doesn't work for multi-byte scancodes
eventPS2 :: (Clock clk)
         => Signal clk (Enabled U8) -> Signal clk (Enabled (Bool, U8))
eventPS2 scancode = runRTL $ do
    releasing <- newReg False
    lastKey <- newReg Nothing

    CASE [ match scancode $ \code -> do
                CASE [ IF (reg releasing) $ do
                            releasing := low
                     , IF (code .==. 0xF0) $ do
                            releasing := high
                     ]
                lastKey := enabledS $ pack (bitNot (reg releasing), code)
         , OTHERWISE $ do
                lastKey := disabledS
         ]

    return $ reg lastKey

keyboard :: forall clk n. (Clock clk, Size n, Num n, Rep n)
         => Matrix n U8
         -> Signal clk (Enabled (Bool, U8))
         -> Signal clk (Matrix n Bool)
keyboard keys lastKey = runRTL $ do
    latches <- forM keys $ \key -> do
        r <- newReg False
        return (key, r)

    whenEnabled lastKey $ \lastKey -> do
        let (pressed, code) = unpack lastKey
        CASE $ Matrix.toList . flip fmap latches $ \(key, r) ->
          IF (code .==. pureS key) $ r := pressed

    return $ pack $ fmap (reg . snd) latches

chip8Keyboard :: (Arcade fabric) => fabric (Seq (Matrix X16 Bool))
chip8Keyboard = do
    (ps2A@PS2{..}, _) <- ps2
    return $ keyboard codes $ eventPS2 . decodePS2 . samplePS2 $ ps2A
  where
    codes :: Matrix X16 U8
    codes = matrix [ 0x16, 0x1E, 0x26, 0x25
                   , 0x15, 0x1D, 0x24, 0x2D
                   , 0x1C, 0x1B, 0x23, 0x2B
                   , 0x1A, 0x22, 0x21, 0x2A
                   ]

testBench :: (Arcade fabric) => fabric ()
testBench = do
    kb <- chip8Keyboard
    leds $ unpack kb `Matrix.cropAt` 0

main :: IO ()
main = do
    emitBench "Input" testBench
