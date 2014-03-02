{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CPU where

import Types
import Utils
import ALU (alu, bcd)

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix (Matrix, matrix)
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits
import Control.Monad

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data CPUIn clk = CPUIn{ cpuMemD :: Signal clk Byte
                      , cpuStart :: Signal clk Bool
                      , cpuVBlank :: Signal clk Bool
                      , cpuFBD :: Signal clk PixData
                      , cpuKeys :: Signal clk (Matrix X16 Bool)
                      }

data CPUOut clk = CPUOut{ cpuMemA :: Signal clk Addr
                        , cpuMemW :: Signal clk (Enabled Byte)
                        , cpuFBA :: Signal clk (VidX, VidY)
                        , cpuFBW :: Signal clk (Enabled PixData)
                        , cpuSound :: Signal clk Bool
                        , cpuOp :: Signal clk (Byte, Byte)
                        , cpuState :: Signal clk State
                        , cpuWaitPixel :: Signal clk Bool
                        }

foo :: CPUOut CLK
foo = cpu $ CPUIn{ cpuMemD = funMap prog (cpuMemA foo)
                 , cpuStart = high
                 , cpuVBlank = low
                 , cpuFBD = low
                 , cpuKeys = pureS (matrix $ replicate 16 False)
                 }
  where
    prog :: Addr -> Maybe Byte
    prog 0x200 = Just 0x00
    prog 0x201 = Just 0xe0
    prog 0x202 = Just 0x00
    prog 0x203 = Just 0x00
    prog _ = Nothing

data State = Init
           | Fetch1
           | Fetch2
           | Fetch3
           | Decode
           | Read
           | WriteMem
           | ClearFB
           | Draw
           | WaitKey
           | Halt
           deriving (Show, Eq, Enum, Bounded)

instance Rep State where
    type W State = X4 -- W X11
    newtype X State = XState{ unXState :: Maybe State }

    unX = unXState
    optX = XState
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X11
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X X11
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X11)

instance Arbitrary State where
    arbitrary = elements [minBound..maxBound]

prop_State_Rep :: Maybe State -> Bool
prop_State_Rep = unX . fromRep . toRep . optX === id
  where
    infix 4 ===
    f === g = \x -> f x == g x

nybbles :: Signal clk Byte -> (Signal clk Nybble, Signal clk Nybble)
nybbles sig = (hi, lo)
  where
    hi = unsigned $ sig `shiftR` 4
    lo = unsigned $ sig

cpu :: forall clk. (Clock clk) => CPUIn clk -> CPUOut clk
cpu CPUIn{..} = runRTL $ do
    registerOf <- newArr (Witness :: Witness X16)
    stackAt <- newArr (Witness :: Witness X20)

    pc <- newReg 0x200
    sp <- newReg 0
    ptr <- newReg 0x000
    timer <- newReg 0
    sound <- newReg 0

    s <- newReg Init
    regsIdx <- newReg Nothing
    writeBCD <- newReg Nothing

    nextA <- newReg 0x000
    nextW <- newReg 0

    nextFBA <- newReg (0, 0)
    -- drawOrigin <- newReg (0 :: VidX, 0 :: VidY)
    drawX <- newReg (0 :: U3)
    drawY <- newReg (0 :: U4)
    drawPattern <- newReg (0 :: Byte)
    waitPattern <- newReg False
    waitPixel <- newReg False
    nextPixel <- newReg False

    opHi <- newReg 0
    opLo <- newReg 0
    let (op1, op2) = nybbles (reg opHi)
        (op3, op4) = nybbles (var opLo)
        addr = flip appendS op2 (var opLo)
        imm = var opLo

    let vX = registerOf (unsigned op2)
        vY = registerOf (unsigned op3)
        v0 = registerOf 0x0
        vF = registerOf 0xf
        (carry, res) = alu op4 (reg vX) (reg vY)

    WHEN cpuVBlank $ do
        timer := mux (reg timer .==. 0) (reg timer - 1, 0)
        sound := mux (reg sound .==. 0) (reg sound - 1, 0)

    let done = do
            s := pureS Fetch1
        doneNext = do
            pc := reg pc + 2
            done
        doneSkip = do
            pc := reg pc + 4
            done
        skipIf p = CASE [ IF p doneSkip, OTHERWISE doneNext ]

    let clearScreen = do
            nextFBA := pureS minBound
            nextPixel := low
            s := pureS ClearFB
        ret = do
            sp := reg sp - 1
            pc := reg (stackAt (reg sp - 1))
            done
        jmp dest = do
            pc := dest
            done
        call = do
            stackAt (reg sp) := addr
            pc := addr
            sp := reg sp + 1
            done
        putImm = do
            vX := imm
            doneNext
        addImm = do
            vX := reg vX + imm
            doneNext
        move = do
            vX := res
            whenEnabled carry $ \c -> vF := mux c (0, 1)
            doneNext
        setPtr = do
            ptr := addr
            doneNext
        randomize = do
            -- TODO
            vX := (reg vX + 1) .&. imm
            doneNext
        drawSprite = do
            nextA := reg ptr
            nextFBA := pack (unsigned $ reg vX, unsigned $ reg vY)
            waitPattern := high
            waitPixel := high
            drawX := 0
            drawY := 0
            s := pureS Draw
        getTimer = do
            vX := reg timer
            doneNext
        setTimer = do
            timer := reg vX
            doneNext
        waitKey = do
            s := pureS WaitKey
        setSound = do
            sound := reg vX
            doneNext
        loadFont = do
            -- This assumes the font for value x starts in RAM in address x * 8
            ptr := unsigned ((reg vX .&. 0x0f) `shiftL` 3)
            doneNext
        storeBCD = do
            nextA := reg ptr
            writeBCD := enabledS 0
            s := pureS WriteMem
        saveRegs = do
            nextA := reg ptr
            regsIdx := enabledS 0
            s := pureS WriteMem
        loadRegs = do
            nextA := reg ptr
            regsIdx := enabledS 0
            s := pureS Read
        addPtr = do
            ptr := reg ptr + unsigned (reg vX)
        halt = do
            s := pureS Halt

    switch (reg s)
      [ Init ==> do
             forM_ [minBound..maxBound] $ \i -> do
                 registerOf (pureS i) := 0
             WHEN cpuStart done
      , Fetch1 ==> do
             nextA := reg pc
             s := pureS Fetch2
      , Fetch2 ==> do
             nextA := reg pc + 1
             opHi := cpuMemD
             s := pureS Fetch3
      , Fetch3 ==> do
             nextA := reg pc + 1
             opLo := cpuMemD
             s := pureS Decode
      , Decode ==> do
             switch op1
               [ 0x0 ==> do
                      switch (var opLo)
                        [ 0xe0 ==> clearScreen
                        , 0xee ==> ret
                        , oTHERWISE halt
                        ]
               , 0x1 ==> jmp addr
               , 0x2 ==> call
               , 0x3 ==> skipIf (reg vX .==. imm)
               , 0x4 ==> skipIf (reg vX ./=. imm)
               , 0x5 ==> switch op4
                 [ 0x0 ==> skipIf (reg vX .==. reg vY)
                 , oTHERWISE halt
                 ]
               , 0x6 ==> putImm
               , 0x7 ==> addImm
               , 0x8 ==> move
               , 0x9 ==> switch op4
                 [ 0x0 ==> skipIf (reg vX ./=. reg vY)
                 , oTHERWISE halt
                 ]
               , 0xa ==> setPtr
               , 0xb ==> jmp (unsigned (reg v0) + addr)
               , 0xc ==> randomize
               , 0xd ==> drawSprite
               , 0xe ==> switch (var opLo)
                 [ 0x9e ==> skipIf (cpuKeys .!. unsigned (reg vX))
                 , 0xa1 ==> skipIf (bitNot $ cpuKeys .!. unsigned (reg vX))
                 , oTHERWISE halt
                 ]
               , 0xf ==> switch (var opLo)
                 [ 0x07 ==> getTimer
                 , 0x0a ==> waitKey
                 , 0x15 ==> setTimer
                 , 0x18 ==> setSound
                 , 0x1e ==> addPtr
                 , 0x29 ==> loadFont
                 , 0x33 ==> storeBCD
                 , 0x55 ==> saveRegs
                 , 0x65 ==> loadRegs
                 ]
               ]
      , WaitKey ==> do
             CASE [ IF (cpuKeys .!. pureS i) $ do
                         vX := pureS (fromIntegral i)
                         doneNext
                  | i <- [minBound..maxBound]
                  ]
      , ClearFB ==> do
             nextFBA := nextPair (reg nextFBA)
             WHEN (reg nextFBA .==. pureS maxBound) doneNext
      , Draw ==> do
             let x = reg drawX
                 y = reg drawY
             let x' = x + 1
                 newLine = x .==. maxBound
                 y' = mux newLine (y, y + 1)
             CASE [ IF (reg waitPixel) $ do
                         waitPixel := low
                         WHEN (reg waitPattern) $ do
                             drawPattern := cpuMemD
                             waitPattern := low
                  , OTHERWISE $ do
                         drawX := x'
                         drawY := y'
                         WHEN newLine $ do
                             nextA := reg nextA + 1
                             waitPattern := high
                         nextPixel := (reg drawPattern `testABit` 7) `xor2` cpuFBD
                         waitPixel := high
                         drawPattern := reg drawPattern `shiftL` 1
                         nextFBA := pack (unsigned $ reg vX + unsigned x', unsigned $ reg vY + unsigned y')
                  ]

             -- TODO: set vF from cpuFBD
             WHEN (y' .==. op4) doneNext
      , WriteMem ==> CASE
          [ match (reg regsIdx) $ \i -> do
                 nextA := reg nextA + 1
                 nextW := reg (registerOf i)
                 CASE [ IF (i .==. unsigned (reg vX)) $ do
                             regsIdx := disabledS
                             doneNext
                      , OTHERWISE $ do
                             regsIdx := enabledS (i + 1)
                      ]
          , match (reg writeBCD) $ \i -> do
                 nextA := reg nextA + 1
                 nextW := bcd (reg vX) .!. i
                 CASE [ IF (i .==. maxBound) $ do
                             writeBCD := disabledS
                             doneNext
                      , OTHERWISE $ do
                             writeBCD := enabledS (i + 1)
                      ]
          ]
      , Read ==> CASE
          [ match (reg regsIdx) $ \i -> do
                 nextA := reg nextA + 1
                 registerOf i := cpuMemD
                 CASE [ IF (i .==. unsigned (reg vX)) $ do
                             regsIdx := disabledS
                             doneNext
                      , OTHERWISE $ do
                             regsIdx := enabledS (i + 1)
                      ]
          ]
      ]

    let cpuMemA = var nextA
        cpuFBA = reg nextFBA
        cpuSound = reg sound ./=. 0
        cpuMemW = packEnabled (reg s .==. pureS WriteMem) (var nextW)
        cpuFBW =  packEnabled (bitNot (reg waitPixel) .&&. (reg s .==. pureS Draw .||. reg s .==. pureS ClearFB)) (var nextPixel)

        cpuOp = pack (reg opHi, var opLo)
        cpuWaitPixel = reg waitPixel
        cpuState = reg s

    return $ CPUOut{..}

switch :: (Eq a, Rep a, sig ~ Signal c) => sig a -> [(Maybe a, RTL s c ())] -> RTL s c ()
switch r = CASE . map (uncurry $ maybe OTHERWISE toIF)
  where
    toIF x rtl = IF (r .==. pureS x) rtl

(==>) :: (Eq a, Rep a) => a -> RTL s c () -> (Maybe a, RTL s c ())
x ==> rtl = (Just x, rtl)

oTHERWISE :: (Eq a, Rep a) => RTL s c () -> (Maybe a, RTL s c ())
oTHERWISE rtl = (Nothing, rtl)
