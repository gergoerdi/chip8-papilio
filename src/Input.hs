{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Input (main, eventLatch, eventLatchRelease, chip8Keyboard) where

import Video hiding (main)
import Utils

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.PS2
import Hardware.KansasLava.Boards.Papilio.Arcade

import Data.Sized.Matrix (matrix, Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Prelude hiding (sequence, sequence_)
import Data.Traversable (sequence)
import Data.Foldable (sequence_)

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

decodeEvent :: forall clk a n. (Clock clk, Rep a, Eq a, Rep n, Size n, Num n)
            => Matrix n a
            -> Signal clk (Enabled (Bool, a))
            -> Signal clk (Enabled (Bool, n))
decodeEvent keys eevent = packEnabled (en .&&. en') $ pack (pressed, val')
  where
    (en, event) = unpackEnabled eevent
    (pressed, val) = unpack event
    (en', val') = unpackEnabled . foldr check disabledS $ Matrix.assocs keys
      where
        check :: (n, a) -> Signal clk (Enabled n) -> Signal clk (Enabled n)
        check (i, x) s = combineEnabled s $ packEnabled (val .==. pureS x) (pureS i)

eventLatch :: forall clk n. (Clock clk, Rep n, Size n, Num n)
           => Signal clk (Enabled (Bool, n))
           -> Signal clk (Matrix n Bool)
eventLatch event = runRTL $ do
    latches <- sequence (Matrix.forAll (const $ newReg False))

    whenEnabled event $ \event -> do
        let (pressed, key') = unpack event
        CASE [ IF (key' .==. pureS key) $ r := pressed
             | (key, r) <- Matrix.assocs latches
             ]

    return $ pack . fmap reg $ latches

eventLatchRelease :: forall clk n. (Clock clk, Rep n, Size n, Num n)
                  => Signal clk Bool
                  -> Signal clk (Enabled n)
                  -> Signal clk (Matrix n Bool)
                  -> Signal clk (Matrix n Bool)
eventLatchRelease timer release latches = runRTL $ do
    regs <- sequence (Matrix.forAll (const $ newReg False))

    CASE [ IF timer $ do
                sequence_ $ Matrix.zipWith (:=) regs (unpack latches)
         , match release $ \i -> setIdx regs i low
         ]
    return $ pack . fmap reg $ regs

chip8Keycodes :: Matrix X16 U8
chip8Keycodes = matrix
    [ 0x22 -- X

    , 0x16 -- 1
    , 0x1E -- 2
    , 0x26 -- 3
    , 0x15 -- Q
    , 0x1D -- W
    , 0x24 -- E
    , 0x1C -- A
    , 0x1B -- S
    , 0x23 -- D

    , 0x1A -- Z
    , 0x21 -- C
    , 0x25 -- 4
    , 0x2D -- R
    , 0x2B -- F
    , 0x2A -- V
    ]

chip8Keyboard :: (Arcade fabric) => fabric (Seq (Enabled (Bool, X16)), Seq (Matrix X16 Bool))
chip8Keyboard = do
    (ps2A@PS2{..}, _) <- ps2
    let event = decodeEvent chip8Keycodes . eventPS2 . decodePS2 . samplePS2 $ ps2A
    return (event, eventLatch event)

testBench :: (Arcade fabric) => fabric ()
testBench = do
    (_, kb) <- chip8Keyboard
    leds $ unpack kb `Matrix.cropAt` 0

main :: IO ()
main = do
    emitBench "Input" testBench
