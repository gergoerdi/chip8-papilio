{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Types (Addr, Byte, Nybble, VidX, VidY, PixData, FrameBuffer) where

import Data.Sized.Unsigned

type Addr = U12
type Byte = U8
type Nybble = U4

type VidX = U6
type VidY = U5
type PixData = Bool
type FrameBuffer = (VidX, VidY) -> PixData
