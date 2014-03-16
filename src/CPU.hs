{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CPU where

import Types
import Utils
import ALU

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix (Matrix, (!))
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits
import Data.Foldable (forM_)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data CPUIn clk = CPUIn{ cpuMemD :: Signal clk Byte
                      , cpuStart :: Signal clk Bool
                      , cpuVBlank :: Signal clk Bool
                      , cpuFBD :: Signal clk (Enabled PixData)
                      , cpuKeyEvent :: Signal clk (Enabled (Bool, X16))
                      , cpuKeys :: Signal clk (Matrix X16 Bool)
                      }

data CPUOut clk = CPUOut{ cpuMemA :: Signal clk Addr
                        , cpuMemW :: Signal clk (Pipe Addr Byte)
                        , cpuFBA :: Signal clk (Enabled (VidX, VidY))
                        , cpuFBW :: Signal clk (Enabled PixData)
                        , cpuSound :: Signal clk Bool
                        , cpuOp :: Signal clk (Byte, Byte)
                        , cpuState :: Signal clk State
                        , cpuWaitPixel :: Signal clk Bool
                        }

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
    registers <- newRegs (Matrix.forAll (const 0))
    let registerOf = regIdx registers
        setRegister = setIdx registers

    stackAt <- newArr (Witness :: Witness X32)

    pc <- newReg 0x200
    sp <- newReg (0 :: U5)
    ptr <- newReg 0x000
    timer <- newReg 0
    sound <- newReg 0

    s <- newReg Init
    regsIdx <- newReg Nothing
    writeBCD <- newReg Nothing

    nextA <- newReg 0x000
    nextW <- newReg 0

    nextFBA <- newReg Nothing
    -- drawOrigin <- newReg (0 :: VidX, 0 :: VidY)
    drawX <- newReg (0 :: U3)
    drawY <- newReg (0 :: U4)
    drawPattern <- newReg (0 :: Byte)
    waitPattern <- newReg False
    waitPixel <- newReg False
    nextPixel <- newReg Nothing

    opHi <- newReg 0
    opLo <- newReg 0
    let (op1, op2) = nybbles (reg opHi)
        (op3, op4) = nybbles (var opLo)
        addr = flip appendS op2 (var opLo)
        imm = var opLo

    let x = unsigned op2
        y = unsigned op3
        vX = registerOf (x :: Signal clk X16)
        vY = registerOf y
        v0 = registerOf 0x0
        vF = registerOf 0xf
        (carry, res) = alu op4 vX vY
        rnd = lfsr high
        setCarry c = registers ! 0xf := mux c (0, 1)

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
            nextFBA := enabledS $ pureS minBound
            nextPixel := enabledS low
            s := pureS ClearFB
        ret = do
            sp := reg sp - 1
            pc := reg (stackAt (bitwise $ reg sp - 1))
            done
        jmp dest = do
            pc := dest
            done
        call = do
            stackAt (bitwise $ reg sp) := reg pc + 2
            pc := addr
            sp := reg sp + 1
            done
        putImm = do
            setRegister x imm
            doneNext
        addImm = do
            setRegister x (vX + imm)
            doneNext
        move = do
            setRegister x res
            whenEnabled carry setCarry
            doneNext
        setPtr = do
            ptr := addr
            doneNext
        randomize = do
            setRegister x (rnd .&. imm)
            doneNext
        drawSprite = do
            nextA := reg ptr
            nextFBA := enabledS $ pack (unsigned vX, unsigned vY)
            waitPattern := high
            waitPixel := high
            drawX := 0
            drawY := 0
            s := pureS Draw
        getTimer = do
            setRegister x (reg timer)
            doneNext
        setTimer = do
            timer := vX
            doneNext
        waitKey = do
            s := pureS WaitKey
        setSound = do
            sound := vX
            doneNext
        loadFont = do
            -- This assumes the font for value x starts in RAM in address x * 8
            ptr := unsigned ((vX .&. 0x0f) `shiftL` 3)
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
            ptr := reg ptr + unsigned vX
            doneNext
        halt = do
            s := pureS Halt

    switch (reg s)
      [ Init ==> do
             forM_ registers $ \r -> do
                 r := 0
             WHEN cpuStart done
      , Fetch1 ==> do
             nextFBA := disabledS
             nextPixel := disabledS
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
               , 0x3 ==> skipIf (vX .==. imm)
               , 0x4 ==> skipIf (vX ./=. imm)
               , 0x5 ==> switch op4
                 [ 0x0 ==> skipIf (vX .==. vY)
                 , oTHERWISE halt
                 ]
               , 0x6 ==> putImm
               , 0x7 ==> addImm
               , 0x8 ==> move
               , 0x9 ==> switch op4
                 [ 0x0 ==> skipIf (vX ./=. vY)
                 , oTHERWISE halt
                 ]
               , 0xa ==> setPtr
               , 0xb ==> jmp (unsigned v0 + addr)
               , 0xc ==> randomize
               , 0xd ==> drawSprite
               , 0xe ==> switch (var opLo)
                 [ 0x9e ==> skipIf (cpuKeys .!. unsigned vX)
                 , 0xa1 ==> skipIf (bitNot $ cpuKeys .!. unsigned vX)
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
             whenEnabled cpuKeyEvent $ \ev -> do
                 let (pressed, key) = unpack ev
                 WHEN pressed $ do
                     setRegister x $ unsigned key
                     doneNext
      , ClearFB ==> do
             let fbA = enabledVal $ reg nextFBA
             nextFBA := enabledS $ nextPair fbA
             WHEN (fbA .==. pureS maxBound) doneNext
      , Draw ==> do
             let x = reg drawX
                 y = reg drawY
             let x' = x + 1
                 newLine = x .==. pureS maxBound
                 y' = mux newLine (y, y + 1)
             CASE [ IF (reg waitPixel) $ do
                         nextPixel := disabledS
                         waitPixel := low
                         WHEN (reg waitPattern) $ do
                             drawPattern := cpuMemD
                             waitPattern := low
                  , OTHERWISE $ whenEnabled cpuFBD $ \oldPixel -> do
                         drawX := x'
                         drawY := y'
                         WHEN newLine $ do
                             nextA := reg nextA + 1
                             waitPattern := high

                         let thisPixel = reg drawPattern `testABit` 7
                         nextPixel := enabledS $ thisPixel `xor2` oldPixel
                         setCarry $ (vF ./=. 0) .||. (thisPixel .&&. oldPixel)

                         waitPixel := high
                         drawPattern := reg drawPattern `shiftL` 1
                         nextFBA := enabledS $ pack (unsigned vX + unsigned x', unsigned vY + unsigned y')
                         WHEN (y' .==. op4) doneNext
                  ]

      , WriteMem ==> CASE
          [ match (reg regsIdx) $ \i -> do
                 nextA := reg nextA + 1
                 nextW := registerOf i
                 CASE [ IF (i .==. unsigned op2) $ do
                             regsIdx := disabledS
                             doneNext
                      , OTHERWISE $ do
                             regsIdx := enabledS (i + 1)
                      ]
          , match (reg writeBCD) $ \i -> do
                 nextA := reg nextA + 1
                 nextW := bcd vX .!. i
                 CASE [ IF (i .==. pureS maxBound) $ do
                             writeBCD := disabledS
                             doneNext
                      , OTHERWISE $ do
                             writeBCD := enabledS (i + 1)
                      ]
          ]
      , Read ==> CASE
          [ match (reg regsIdx) $ \i -> do
                 nextA := reg nextA + 1
                 setRegister i cpuMemD
                 CASE [ IF (i .==. unsigned op2) $ do
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
        cpuMemW = packEnabled (reg s .==. pureS WriteMem) $ pack (reg nextA, var nextW)
        cpuFBW = var nextPixel

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
