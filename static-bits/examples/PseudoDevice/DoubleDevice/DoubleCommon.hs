-- A utility module for reading/writing entries from the "double device"
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
module DoubleCommon where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Communication.RingBuffer
import RendezvousLib.ClientServer(clientServerRingBufferConnections)

data DoubleRequest = DoubleRequest Word64 Word64
data DoubleResponse = DoubleResponse Word64 Word64

type DoubleFrontRing = FrontRingBuffer DoubleRequest DoubleResponse Word64
type DoubleBackRing = BackRingBuffer DoubleRequest DoubleResponse Word64

client :: IO DoubleFrontRing
(client,listener) = clientServerRingBufferConnections "DoubleDevice"

instance RingBufferable DoubleRequest DoubleResponse Word64 where
  requestId (DoubleRequest id _) = id
  responseId (DoubleResponse id _ ) = id
  entrySize _ _  = 16

instance FrontRingBufferable DoubleRequest DoubleResponse Word64 where
  peekResponse ptr = do 
    id <- peekByteOff (castPtr ptr) 0
    val <- peekByteOff (castPtr ptr) 8
    return $ DoubleResponse id val
  pokeRequest ptr (DoubleRequest id val) = do 
    pokeByteOff (castPtr ptr) 0 id
    pokeByteOff (castPtr ptr) 8 val

instance BackRingBufferable DoubleRequest DoubleResponse Word64 where
  peekRequest ptr = do 
    id <- peekByteOff (castPtr ptr) 0
    val <- peekByteOff (castPtr ptr) 8
    return $ DoubleRequest id val
  pokeResponse ptr (DoubleResponse id val) = do
    pokeByteOff (castPtr ptr) 0 id
    pokeByteOff (castPtr ptr) 8 val
