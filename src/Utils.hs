module Utils (betweenCO, ramWithInit) where

import Language.KansasLava
import Data.Sized.Ix

betweenCO :: (Ord a, Rep a) => Signal clk a -> (a, a) -> Signal clk Bool
x `betweenCO` (lo, hi) = pureS lo .<=. x .&&. x .<. pureS hi

ramWithInit :: (Clock clk, Size a, Eq a, Rep a, Bounded a, Rep d)
            => (Signal clk a -> Signal clk a)
            -> (Signal clk a -> Signal clk d)
            -> Signal clk (Pipe a d)
            -> Signal clk (a -> d)
ramWithInit next rom pipe = runRTL $ do
    x <- newReg minBound
    x' <- newReg minBound
    filling <- newReg True
    reading <- newReg False

    CASE
      [ IF (reg reading) $ do
             x := reg x'
             reading := low
             filling := reg x ./=. maxBound
      , IF (reg filling) $ do
             x' := next (reg x)
             reading := high
      ]

    let (we, writeLine) = unpack pipe
        we' = reg reading .||. we
        writeLine' = mux (reg filling) (writeLine, pack (reg x, rom (reg x)))

    return $ writeMemory $ packEnabled we' writeLine'
