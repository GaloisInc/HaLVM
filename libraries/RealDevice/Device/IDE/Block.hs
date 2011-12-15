-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.IDE.Block where

import Device.Bits
import Device.IDE.API
import Device.IDE.Info
import Device.IDE.DMA

import Control.Concurrent(takeMVar)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peek,poke)
import Foreign.Marshal.Array(advancePtr)

import Hypervisor.Memory(VPtr)

-- in bytes
block_size :: Word16
block_size  = 4096

-- in bytes
sectors_per_block :: Word16
sectors_per_block = block_size `div` sector_size

block_to_sector :: Word32 -> Word32
block_to_sector x = x * fromIntegral sectors_per_block

pio_read_block :: Resources -> Word1 -> Word32 -> Ptr Word16
           -> IO (Word32, Maybe Error)
pio_read_block r dev block_num buffer =
  do select_device r dev
     set_sector_count_28 r (fromIntegral sectors_per_block)
     set_lba_28 r dev (fromIntegral (block_to_sector block_num))
     set_command r 0x20
     pio get_word r buffer
  where get_word buf = do w <- get_data_port r
                          poke buf w
                          return (advancePtr buf 1)

pio_write_block :: Resources -> Word1 -> Word32 -> Ptr Word16
            -> IO (Word32, Maybe Error)
pio_write_block r dev block_num buffer =
  do select_device r dev
     set_sector_count_28 r (fromIntegral sectors_per_block)
     set_lba_28 r dev (block_to_sector block_num)
     set_command r 0x30
     pio set_word r buffer
  where set_word buf = do w <- peek buf
                          set_data_port r w
                          return (advancePtr buf 1)

read_block :: Device -> Word32 -> VPtr a -> IO (Maybe Error)
read_block dev block_num buff =
  do let sector = block_num * fromIntegral sectors_per_block
     dma dma_read dev sector (fromIntegral sectors_per_block) [(block_size,buff)]
     takeMVar (dma_done (resources dev))

write_block :: Device -> Word32 -> VPtr a -> IO (Maybe Error)
write_block dev block_num buff =
  do let sector = block_num * fromIntegral sectors_per_block
     dma dma_write dev sector (fromIntegral sectors_per_block) [(block_size,buff)]
     takeMVar (dma_done (resources dev))


-- XXX: do it with DMA
-- XXX: should we use the interrupt for PIO?


