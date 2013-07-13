-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Hypervisor.IOPorts
  ( IOPort
  , in8, in16, in32
  , out8, out16, out32
  ) where

import Data.Word

type IOPort = Word16

foreign import ccall unsafe "io.h in8"  in8  :: IOPort -> IO Word8
foreign import ccall unsafe "io.h in16" in16 :: IOPort -> IO Word16
foreign import ccall unsafe "io.h in32" in32 :: IOPort -> IO Word32

foreign import ccall unsafe "io.h out8"  out8  :: IOPort -> Word8  -> IO ()
foreign import ccall unsafe "io.h out16" out16 :: IOPort -> Word16 -> IO ()
foreign import ccall unsafe "io.h out32" out32 :: IOPort -> Word32 -> IO ()


