{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module ALU (alu, bcd) where

import Language.KansasLava
import Data.Sized.Matrix (Matrix, matrix)
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits

alu :: forall clk. (Clock clk)
    => Signal clk U4
    -> Signal clk U8
    -> Signal clk U8
    -> (Signal clk (Enabled Bool), Signal clk U8)
alu sel x y = unpack $ ops .!. bitwise sel
  where
    ops :: Signal clk (Matrix X16 (Enabled Bool, U8))
    ops = pack $ matrix . map (\f -> pack $ f x y) $
          [ idS        -- 0x0
          , orS        -- 0x1
          , andS       -- 0x2
          , xorS       -- 0x3
          , addS       -- 0x4
          , subS       -- 0x5
          , shiftRS    -- 0x6
          , subS'      -- 0x7
          , unused     -- 0x8
          , unused     -- 0x9
          , unused     -- 0xa
          , unused     -- 0xb
          , unused     -- 0xc
          , unused     -- 0xd
          , shiftLS    -- 0xe
          , unused     -- 0xf
          ]

    idS _ y = noCarry y
    orS x y = noCarry $ x .|. y
    andS x y = noCarry $ x .&. y
    xorS x y = noCarry $ x `xor` y
    addS x y = let z = x + y in carry (z .<. x) z
    subS x y = let z = x - y in carry (z .<=. x) z
    shiftRS x _ = carry (x `testABit` 0) (x `shiftR` 1)
    subS' x y = let z = y - x in carry (z .<=. y) z
    shiftLS x _ = carry (x `testABit` 7) (x `shiftL` 1)
    unused _ _ = noCarry 0

    noCarry z = (disabledS, z)
    carry c z = (enabledS c, z)

bcd :: forall clk. (Clock clk)
    => Signal clk U8
    -> Signal clk (Matrix X3 U8)
bcd = funMap (Just . toBCD)
  where
    toBCD :: U8 -> Matrix X3 U8
    toBCD x = matrix [x `div` 100, (x `div` 10) `mod` 10, x `mod` 10]
