-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
-- | This module defines the type of page map entries.

{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module DomainBuilder.PMapEntry
  ( Entry
  , entry, get_flag, set_flag, set_flags, get_mfn, set_mfn
  , Flag, present,  writeable, user_mode, write_trough, cache_disabled
        , accessed, dirty, pat1, global
        , large, pat2
  , access_flags
  , unmapped
  , page_bits, entry_bits
  )
  where

import Hypervisor.Memory
import DomainBuilder.Utils
import DomainBuilder.IfaceTypes(Access(..))
import DomainBuilder.Utils

import Data.Word
import Data.Bits
import Data.List
import Foreign.Storable(Storable)

-- In 64-bit mode:
-- newtype Entry = Entry Word64 deriving (Eq,Storable)
#ifdef CONFIG_X86_PAE
-- The type of entries in the page mapping strucutres.
newtype Entry = Entry Word64 deriving (Eq,Storable)
#else
newtype Entry = Entry Word32 deriving (Eq,Storable)
#endif

instance Show Entry where
  show (Entry x) = show_hex x

-- | The number of bits needed to index a machine page.
-- XXX: Move somewhere else?
page_bits :: Int
page_bits = bytes_to_bits pageSize

-- | The number of bits used by an entry in a page map.
entry_bits :: Int
entry_bits = bitSize x
  where Entry x = error "to determine the size of entries"




-- Working with the flags in an entry ------------------------------------------

-- The entries in a page map all have roughly the same shape:
--   * The physical address of a machine page (or 2/4MB super page)
--   * A bunch of configuration flags in the least signficant bits.
--
-- The flags share bits with the phisical address because the phisical
-- addresses are aligned and so, their least signifiacant bits are known
-- to be 0.


-- Note that not all flags are applicable for all entries.
-- We could do more with the types, if we deemed it necessary.
-- Reference: "Intel® 64 and IA-32 Architectures Software Developer’s Manual"
--            Volume 3A, Chapter 3

-- | Encapsulates valid flags.
-- Not perfect but helps to avoid silly mistakes.
newtype Flag = Flag Int deriving Show

present, writeable, user_mode, write_trough, cache_disabled
       , accessed , dirty    , pat1        , global
       , large, pat2
       :: Flag

present : writeable : user_mode : write_trough : cache_disabled
        : accessed  : dirty     : pat1         : global
        : _
  = map Flag [0..]

large = Flag 7
pat2  = Flag 12


-- | Get a flag for an entry.
get_flag :: Entry -> Flag -> Bool
get_flag (Entry x) (Flag n)  = testBit x n

-- | Set one flag on an entry.
set_flag :: Entry -> Flag -> Entry
set_flag (Entry x) (Flag n) = Entry (setBit x n)

-- | Set the given flags in the entry.
set_flags :: Entry -> [Flag] -> Entry
set_flags e xs = foldl' set_flag e xs

-- | A blank entry.
unmapped :: Entry
unmapped = Entry 0

-- | Gets the machine frame stored in a page.
-- WARNING: This is only for 4K pages, not super pages.
get_mfn :: Entry -> MFN
get_mfn (Entry x) = toMFN (fromIntegral (x `shiftR` page_bits))

-- | Set the physical pointer for an entry to a given machine page.
-- WARNING: This is only for 4K pages, not super pages.
set_mfn :: Entry -> MFN -> Entry
set_mfn (Entry x) y = Entry (upper .|. x .&. mask)
  where mask    = fromIntegral (pageSize - 1)
        upper   = fromIntegral (mptrToInteger (mfnToMPtr y))

entry :: MFN -> [Flag] -> Entry
entry mfn flags = set_mfn (set_flags unmapped flags) mfn

access_flags :: Access -> [Flag]
access_flags access = case access of
  Access_readWrite -> [present,writeable]
  Access_readOnly  -> [present]
  Access_none      -> []    -- May lead to gaps in mapping!


