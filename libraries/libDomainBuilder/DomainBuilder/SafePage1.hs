-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
-- | Provides an array like interface to memory pages.
module DomainBuilder.SafePage1
  ( Page, entry_num, get_entry, set_entry, set_entries
  , map_page, unmap_page, with_page
  , unsafe_ptr, unsafe_mk
  , zero_page, copy_bs
  , dump
  )
  where

import Hypervisor.Basics
import Hypervisor.Memory

import DomainBuilder.Utils

import Data.ByteString.Internal(memcpy,toForeignPtr)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Foreign.Storable
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(plusPtr)

newtype Page a = Page (VPtr a)

unsafe_ptr :: Page a -> VPtr a
unsafe_ptr (Page p) = p

unsafe_mk :: VPtr a -> Page a
unsafe_mk p = Page p

entry_num :: Storable a => Page a -> Word
entry_num (Page p) = fromIntegral pageSize `div`
                                            fromIntegral (sizeOf (pointee p))
  where pointee :: VPtr a -> a
        pointee _ = error "(unevalueted) Size of page element"

-- | Make sure that index is within the array.
-- We use modulo arithmetic to ensure this.
-- Note that this function will crash if we use
-- it with entries whose size is larger then a page size
-- because we will be dividing by 0.
-- PRE: The size of the Storable is less then a page.
safe_ix :: Storable a => Page a -> Word -> Int
safe_ix p x = fromIntegral (mod x (entry_num p))

get_entry :: Storable a => Page a -> Word -> IO a
get_entry pp@(Page p) x   = peekElemOff p (safe_ix pp x)

set_entry :: Storable a => Page a -> Word -> a -> IO ()
set_entry pp@(Page p) x e = pokeElemOff p (safe_ix pp x) e

set_entries :: Storable a => Page a -> [(Word,a)] -> IO ()
set_entries p xs = mapM_ (\(x,e) -> set_entry p x e) xs

-- | Fills a page with 0s.
zero_page :: Page Word8 -> IO ()
zero_page (Page p) = mapM_ (\i -> pokeByteOff p i (0::Word8))
                                          [0 .. fromIntegral pageSize - 1]

-- | Fills a page with data from a bytestring.
-- It copies at most 'pageSize' bytes.
-- If the string contains less then 'pageSize' bytes,
-- then the remaining bytes are left as they are.
copy_bs :: BS.ByteString -> Page Word8 -> IO ()
copy_bs from (Page to) = withForeignPtr fromFP $ \f ->
                            memcpy to (f `plusPtr` o) len
  where (fromFP,o,l) = toForeignPtr from
        len          = min (fromIntegral l) (fromIntegral pageSize)


-- | Map a given machine frame into our address space.
-- We turn errors into IO exceptions.
map_page :: DomId -> MFN -> IO (Page a)
map_page dom mfn = Page `fmap` ignoreErrors (mapForeignMachineFrames dom [mfn])

-- | Remove a mapped page from our address space.
-- WARNING: We should not read\/write to the pagemap after this.
unmap_page :: Page a -> IO ()
unmap_page (Page p) = unmapForeignMachineFrames p (fromIntegral pageSize)

-- | Map a page, apply a function to it, then unmap it.
-- WARNING: The page value should not be used after we return from the function.
with_page :: DomId -> MFN -> (Page a -> IO b) -> IO b
with_page dom mfn f = do p <- map_page dom mfn
                         a <- f p
                         unmap_page p
                         return a

dump :: (Storable a, Bits a, Integral a) => Page a -> IO String
dump p = do ws <- mapM (get_entry p) [ 0 .. entry_num p - 1 ]
            return $ unlines $ map unwords $ chunks 8
                   $ map show_hex ws


