-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.DomainInfo(
         DomId
       , toDomId, fromDomId
       , SID
       , DomainHandle
       , VCPU
       , fromVCPU
       )
 where

import Data.Word
import Foreign.Storable

newtype DomId = DomId Word32
instance Eq       DomId
instance Show     DomId

newtype SID = SID Word32
instance Eq       SID
instance Show     SID
instance Storable SID

newtype DomainHandle = DomHandle [Word8]
instance Eq       DomainHandle
instance Show     DomainHandle
instance Storable DomainHandle

newtype VCPU = VCPU Word

toDomId :: Integral a => a -> DomId
fromDomId :: Integral a => DomId -> a
fromVCPU :: Integral a => VCPU -> a
