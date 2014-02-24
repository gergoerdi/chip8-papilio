{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Input (main, prop_PS2State_Rep) where

import Video hiding (main)

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Sized.Matrix (matrix, Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits
import Data.Traversable (forM)

data PS2State = Idle
              | Shift
              | Parity
              | Stop
              deriving (Show, Eq, Enum, Bounded)

instance Rep PS2State where
    type W PS2State = X2
    newtype X PS2State = XPS2State{ unXPS2State :: Maybe PS2State }

    unX = unXPS2State
    optX = XPS2State

    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X4
        s' = fmap (fromIntegral . fromEnum) $ unX s

    fromRep rep = optX $ fmap (toEnum . fromIntegral) $ unX x
      where
        x :: X X4
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X4)

instance Arbitrary PS2State where
    arbitrary = elements [minBound..maxBound]

prop_PS2State_Rep :: PS2State -> Bool
prop_PS2State_Rep = unX . fromRep . toRep . optX . Just === Just
  where
    infix 4 ===
    f === g = \x -> f x == g x

debouncePS2 :: (Clock clk) => PS2 clk -> Signal clk (Enabled Bool)
debouncePS2 PS2{..} = runRTL $ do
    ps2Clock' <- newReg False
    clockPattern <- newReg (0 :: U8)

    ps2Data' <- newReg False
    dataPattern <- newReg (0 :: U8)

    let fallingClock = reg ps2Clock' .&&. bitNot (var ps2Clock')

    clockPattern := (reg clockPattern `shiftL` 1) .|. unsigned ps2Clock
    CASE [ IF (reg clockPattern .==. pureS maxBound) $ do
                ps2Clock' := high
         , IF (reg clockPattern .==. pureS minBound) $ do
                ps2Clock' := low
                -- WHEN (reg ps2Clock') $ fallingClock := high
         ]

    dataPattern := (reg dataPattern `shiftL` 1) .|. unsigned ps2Data
    CASE [ IF (reg dataPattern .==. pureS maxBound) $ do
                ps2Data' := high
         , IF (reg dataPattern .==. pureS minBound) $ do
                ps2Data' := low
         ]

    return $ packEnabled fallingClock (reg ps2Data')

parity :: forall clk n. (Clock clk, Size n, Rep n, Integral n, Enum n)
       => Signal clk (Unsigned n) -> Signal clk Bool
parity x = foldr xor2 low $ map (testABit x . pureS) [minBound .. maxBound :: n]

drivePS2 :: (Clock clk) => Signal clk (Enabled Bool) -> Signal clk (Enabled U8)
drivePS2 line = runRTL $ do
    state <- newReg Idle
    shiftCounter <- newReg (0 :: X8)
    shift <- newReg (0 :: U8)

    parityChecked <- newReg False
    haveCode <- newReg False
    let enableOutput = var haveCode .&&. isEnabled line

    whenEnabled line $ \ps2Data -> do
        CASE
          [ IF (reg state .==. pureS Idle) $ do
                 state := mux ps2Data (pureS Shift, pureS Idle)
                 haveCode := low
                 shift := 0
          , IF (reg state .==. pureS Shift) $ do
                 let last = reg shiftCounter .==. pureS maxBound
                 state := mux last (pureS Shift, pureS Parity)
                 shiftCounter := mux last (reg shiftCounter + 1, 0)
                 shift := (reg shift `shiftR` 1) .|. (unsigned ps2Data `shiftL` 7)
          , IF (reg state .==. pureS Parity) $ do
                 state := pureS Stop
                 parityChecked := ps2Data `xor2` parity (reg shift)
          , IF (reg state .==. pureS Stop) $ do
                 state := pureS Idle
                 haveCode := ps2Data .&&. reg parityChecked
          ]

    return $ packEnabled enableOutput (reg shift)

whenEnabled sig = CASE . return . match sig

-- TODO: This doesn't work for multi-byte scancodes
decodePS2 :: (Clock clk)
          => Signal clk (Enabled U8) -> Signal clk (Enabled (Bool, U8))
decodePS2 scancode = runRTL $ do
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

testBench :: (Arcade fabric) => fabric ()
testBench = do
    (ps2A@PS2{..}, _) <- ps2
    let scancode = decodePS2 . drivePS2 . debouncePS2 $ ps2A
        kb = keyboard codes scancode

    leds $ unpack kb `Matrix.cropAt` 0
  where
    codes :: Matrix X16 U8
    codes = matrix [ 0x16, 0x1e, 0x26, 0x25
                   , 0x15, 0x22, 0x24, 0x2D
                   , 0x1C, 0x1B, 0x23, 0x2B
                   , 0x1A, 0x22, 0x21, 0x2A
                   ]

main :: IO ()
main = do
    emitBench "Input" testBench
