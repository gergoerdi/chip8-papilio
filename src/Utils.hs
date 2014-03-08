module Utils
       ( betweenCO
       , ramWithInit
       , nextPair
       , combineEnabled
       , newRegs, setIdx, regIdx, varIdx
       ) where

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Prelude hiding (mapM)
import Data.Traversable (mapM)

betweenCO :: (Ord a, Rep a) => Signal clk a -> (a, a) -> Signal clk Bool
x `betweenCO` (lo, hi) = pureS lo .<=. x .&&. x .<. pureS hi

ramWithInit :: (Clock clk, Size a, Eq a, Rep a, Bounded a, Rep d)
            => (Signal clk a -> Signal clk a)
            -> (Signal clk a -> Signal clk d)
            -> Signal clk (Pipe a d)
            -> (Signal clk (a -> d), Signal clk Bool)
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

    return (writeMemory $ packEnabled we' writeLine', reg filling)

nextPair :: (Size a, Size b)
         => Signal clk (Unsigned a, Unsigned b)
         -> Signal clk (Unsigned a, Unsigned b)
nextPair xy = pack (x + 1, mux nextRow (y, y + 1))
  where
    (x, y) = unpack xy
    nextRow = x .==. maxBound

combineEnabled :: (Clock clk, Rep a)
               => Signal clk (Enabled a)
               -> Signal clk (Enabled a)
               -> Signal clk (Enabled a)
combineEnabled s1 s2 = packEnabled (en1 .||. en2) (mux en1 (v2, v1))
  where
    (en1, v1) = unpackEnabled s1
    (en2, v2) = unpackEnabled s2

newRegs :: (Clock clk, Rep a, Size n)
        => Matrix n a -> RTL s clk (Matrix n (Reg s clk a))
newRegs inits = mapM newReg inits

setIdx :: (Clock clk, Rep a, Rep n, Size n)
       => Matrix n (Reg s clk a) -> Signal clk n -> Signal clk a -> RTL s clk ()
setIdx regs idx val = CASE [ IF (idx .==. pureS i) $ r := val
                           | (i, r) <- Matrix.assocs regs
                           ]

regIdx :: (Clock clk, Rep a, Rep n, Size n)
       => Matrix n (Reg s clk a) -> Signal clk n -> Signal clk a
regIdx regs idx = pack (fmap reg regs) .!. idx

varIdx :: (Clock clk, Rep a, Rep n, Size n)
       => Matrix n (Reg s clk a) -> Signal clk n -> Signal clk a
varIdx regs idx = pack (fmap var regs) .!. idx
