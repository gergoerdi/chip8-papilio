{-# LANGUAGE RecordWildCards #-}
module VGA640x480
       ( VGADriverIn(..)
       , VGADriverOut(..)
       , driveVGA
       ) where

import Utils

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.VGA as VGA

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix

data VGADriverIn clk r g b = VGADriverIn
                             { vgaInReset :: Signal clk Bool
                             , vgaInR :: Signal clk (Unsigned r)
                             , vgaInG :: Signal clk (Unsigned g)
                             , vgaInB :: Signal clk (Unsigned b)
                             }

data VGADriverOut clk r g b = VGADriverOut
                              { vgaOut :: VGA clk r g b
                              , vgaOutClkPhase :: Signal clk Bool
                              , vgaOutX :: Signal clk (Enabled U10)
                              , vgaOutY :: Signal clk (Enabled U10)
                              }

driveVGA :: (Clock clk, Size r, Size g, Size b)
         => VGADriverIn clk r g b -> VGADriverOut clk r g b
driveVGA VGADriverIn{..} = runRTL $ do
    phase <- newReg False
    hCount <- newReg (0 :: U10)
    vCount <- newReg (0 :: U10)

    let hEnd = reg hCount .==. pureS hMax
        vEnd = reg vCount .==. pureS vMax

    CASE
      [ IF vgaInReset $ do
             phase := low
             hCount := 0
             vCount := 0
      , OTHERWISE $ do
             phase := bitNot (reg phase)
             WHEN (reg phase) $ do
                 hCount := mux hEnd (reg hCount + 1, 0)
                 WHEN hEnd $ do
                     vCount := mux vEnd (reg vCount + 1, 0)
      ]

    let hsync = pureS hSyncStart .<=. reg hCount .&&.
                reg hCount .<. pureS hSyncEnd
        vsync = pureS vSyncStart .<=. reg vCount .&&.
                reg vCount .<. pureS vSyncEnd

    let hVisible = reg hCount .<. pureS hSize
        vVisible = reg vCount .<. pureS vSize
        visible = hVisible .&&. vVisible

    let vgaOutClkPhase = reg phase
        vgaOutX = packEnabled visible (reg hCount)
        vgaOutY = packEnabled visible (reg vCount)
        vgaOut = VGA{ vgaR = mux visible (0, vgaInR)
                    , vgaG = mux visible (0, vgaInG)
                    , vgaB = mux visible (0, vgaInB)
                    , vgaHSync = bitNot hsync
                    , vgaVSync = bitNot vsync
                    }

    return VGADriverOut{..}
  where
    hSize = 640 :: U10
    hPre = 16
    hSync = 96
    hSyncStart = hSize + hPre
    hSyncEnd = hSyncStart + hSync
    hPost = 48
    hMax = sum [hSize, hPre, hSync, hPost] - 1

    vSize = 480 :: U10
    vPre = 10
    vSync = 2
    vSyncStart = vSize + vPre
    vSyncEnd = vSyncStart + vSync
    vPost = 33
    vMax = sum [vSize, vPre, vSync, vPost] - 1
