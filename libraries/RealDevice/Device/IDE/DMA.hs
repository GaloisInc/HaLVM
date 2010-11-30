-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.IDE.DMA
  ( Table
  , alloc_table
  , phys_addr
  , fill_table
  , dump_table
  , DMACommand, dma_code, dma_read, dma_write
  ) where

import Prelude hiding (putStrLn)

import Hypervisor.Basics(ignoreErrors,xOnException)
import Hypervisor.Memory(allocPage,virtualToMachine,VPtr,MPtr,mptrToPtr)
import Control.Monad(when)
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable(peek,poke)
import Foreign.Marshal.Array(advancePtr)
import Numeric(showHex)


newtype DMACommand = Command { dma_code :: Word8 }

dma_read :: DMACommand
dma_read  = Command 0xC8

dma_write :: DMACommand
dma_write  = Command 0xCA


-- Should be DWord aligned.  For now we just use a 4k page.
newtype Table = Table (Ptr Word32)

-- | Allocate 4k for a DMA descriptor table.
-- If there is not enogh memory, then we raise an exception.
alloc_table :: IO Table
alloc_table = do
   page <- allocPage `xOnException` ioError (userError "alloc_dma_table: out of memory")
   return (Table (castPtr page))

-- | Get the physycal address of a DMA table.
phys_addr :: Table -> IO (MPtr a)
phys_addr (Table x) = ignoreErrors $ virtualToMachine (castPtr x)


-- | Write the entries for a DMA descriptor table.
-- The parameter list should contain buffer sizes, paired with buffers.
--   * /Warning:/ The argument list should not be empty.
--   * /Warning:/ The list should have at most 512 elements.
fill_table :: Table -> [(Word16, VPtr a)] -> IO ()
fill_table (Table p) xs = init' p xs
  where init' _ [] = ioError (userError "dma_table_fill: no entries")
        init' entry ((size,buffer):rxs) =
          do let size1 = fromIntegral size
                 islast  = null rxs
                 size2 = if islast then size1 `setBit` 31 else size1
             physaddr <- ignoreErrors $ virtualToMachine buffer
             poke entry (fromIntegral (ptrToWordPtr (mptrToPtr physaddr)))
             poke (advancePtr entry 1) size2
             when (not islast) (init' (advancePtr entry 2) rxs)

-- | For debugging purposes.
dump_table :: (String -> IO ()) -> Table -> IO ()
dump_table putStrLn (Table _p) = dump_entry _p
  where dump_entry p = do addr <- peek p
                          size <- peek (advancePtr p 1)
                          putStrLn ("addr: " ++ showHex addr "" ++ ", size: " ++
                                                        show (size .&. 0xFFFF))
                          when (not (size `testBit` 31)) $
                            dump_entry (p `advancePtr` 2)





