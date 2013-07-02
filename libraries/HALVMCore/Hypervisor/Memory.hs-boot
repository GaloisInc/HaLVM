-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Memory(PFN,VPtr,toPFN,MFN,toMFN,fromMFN) where

import Foreign.Ptr
import Data.Word

type VPtr a = Ptr a

newtype PFN = PFN Word
newtype MFN = MFN Word

instance Eq MFN
instance Show MFN

toPFN   :: Integral a => a -> PFN
toMFN   :: Word -> MFN
fromMFN :: MFN -> Word
