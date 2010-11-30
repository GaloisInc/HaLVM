-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- Blank for Haddock.
-- | Some accessor functions for the start info page.
module Hypervisor.StartInfo(StartInfoPtr(..), getStartInfo, getModStart, getModLen) where

#include <xen/xen.h>
#include <hbmxen.h>

import Data.Word(Word32)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peekByteOff)

newtype StartInfoPtr = StartInfoPtr { unStartInfoPtr :: Ptr Word32 }
  deriving (Show,Eq)

foreign import ccall unsafe "start_info.h get_start_info" getStartInfo :: IO StartInfoPtr

getModStart :: StartInfoPtr -> IO Word32
getModStart = (#peek start_info_t,mod_start) . unStartInfoPtr

getModLen :: StartInfoPtr -> IO Word32
getModLen = (#peek start_info_t,mod_len) . unStartInfoPtr
