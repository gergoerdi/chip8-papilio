{-# LANGUAGE RecordWildCards #-}
module VGADriver
       ( VGAParams(..)
       , VGATiming(..)
       , VGADriverIn(..)
       , VGADriverOut(..)
       , driveVGA
       , vga640x480at60
       ) where

-- TODO: Move this whole module to Hardware.KansasLava.VGA.Driver

import Utils

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Hardware.KansasLava.VGA as VGA

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix

data VGADriverIn clk r g b = VGADriverIn
                             { vgaInReset :: Signal clk Bool
                             , vgaInR :: Signal clk r
                             , vgaInG :: Signal clk g
                             , vgaInB :: Signal clk b
                             }

data VGADriverOut clk w h r g b = VGADriverOut
                                  { vgaOut :: VGA clk r g b
                                  -- , vgaOutClkPhase :: Signal clk Bool
                                  , vgaOutX :: Signal clk (Enabled (Unsigned w))
                                  , vgaOutY :: Signal clk (Enabled (Unsigned h))
                                  }

data VGAParams w h = VGAParams
                     { vgaHorizTiming :: VGATiming w
                     , vgaVertTiming :: VGATiming h
                     }

data VGATiming a = VGATiming{ visibleSize, pre, syncPulse, post :: Unsigned a }

driveVGA :: (Clock clk, Rep r, Rep g, Rep b, Size w, Size h)
         => VGAParams w h
         -> VGADriverIn clk r g b
         -> VGADriverOut clk w h r g b
driveVGA VGAParams{..} VGADriverIn{..} = runRTL $ do
    phase <- newReg False
    hCount <- newReg 0
    vCount <- newReg 0

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
        vgaOut = VGA{ vgaR = packEnabled visible vgaInR
                    , vgaG = packEnabled visible vgaInG
                    , vgaB = packEnabled visible vgaInB
                    , vgaHSync = bitNot hsync
                    , vgaVSync = bitNot vsync
                    }

    return VGADriverOut{..}
  where
    hSize = visibleSize vgaHorizTiming
    hPre = pre vgaHorizTiming
    hSync = syncPulse vgaHorizTiming
    hSyncStart = hSize + hPre
    hSyncEnd = hSyncStart + hSync
    hPost = post vgaHorizTiming
    hMax = sum [hSize, hPre, hSync, hPost] - 1

    vSize = visibleSize vgaVertTiming
    vPre = pre vgaVertTiming
    vSync = syncPulse vgaVertTiming
    vSyncStart = vSize + vPre
    vSyncEnd = vSyncStart + vSync
    vPost = post vgaVertTiming
    vMax = sum [vSize, vPre, vSync, vPost] - 1

-- | VGA 640*480@60Hz
-- Assumes a clock of 25.175 MHz
vga640x480at60 :: VGAParams X10 X10
vga640x480at60 = VGAParams{ vgaHorizTiming = VGATiming 640 16 96 48
                          , vgaVertTiming  = VGATiming 480 16  2 33
                          }
