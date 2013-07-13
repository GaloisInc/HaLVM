{-# LANGUAGE MultiParamTypeClasses #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
module ROT13Ring where

import Data.Word
import Foreign.Storable
import Hypervisor.Memory
import Communication.RingBuffer
import RendezvousLib.ClientServer(clientServerRingBufferConnections)

data ROT13Request = ROT13Request Word64 GrantRef Word16
data ROT13Response = ROT13Response Word64 Word32

type FrontROT13Ring = FrontRingBuffer ROT13Request ROT13Response Word64
type BackROT13Ring = BackRingBuffer ROT13Request ROT13Response Word64

client :: IO (FrontROT13Ring)
(client,server) = clientServerRingBufferConnections "ROT13Device"

instance RingBufferable ROT13Request ROT13Response Word64 where
    requestId (ROT13Request id _ _) = id
    responseId (ROT13Response id _) = id
    entrySize _ _ = 16

instance FrontRingBufferable ROT13Request ROT13Response Word64 where
    peekResponse ptr = do
      id <- peekByteOff ptr 0
      code <- peekByteOff ptr 8
      return $ ROT13Response id code
    pokeRequest ptr (ROT13Request id (GrantRef gref) size) = do
      pokeByteOff ptr 0 id
      pokeByteOff ptr 8 gref
      pokeByteOff ptr 12 size

instance BackRingBufferable ROT13Request ROT13Response Word64 where
    peekRequest ptr = do
      id <- peekByteOff ptr 0
      grefnum <- peekByteOff ptr 8
      size <- peekByteOff ptr 12
      return $ ROT13Request id (GrantRef grefnum) size
    pokeResponse ptr (ROT13Response id code) = do
      pokeByteOff ptr 0 id
      pokeByteOff ptr 8 code

