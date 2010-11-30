-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
-- | This module initializes the page tables for a child domain.

{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module DomainBuilder.PageMap
  ( write_pmaps
  , VPage(..), vpage_to_word, incVPage  -- XXX: Move somewhere?
  , stack_pages
  , dump_pmap
  )
  where

import Hypervisor.Basics
import Hypervisor.Memory
import Hypervisor.Debug
import DomainBuilder.SafePage1
import DomainBuilder.Utils
import DomainBuilder.PMapEntry
import DomainBuilder.IfaceTypes(Access(..))

import Data.Word
import Data.Bits
import qualified Data.Array
import Data.List
import qualified Data.Map as Map
import Control.Monad

-- XXX: There is a fair bit of duplication in the init of the 3 levels.
-- We could probably factor out the common bits.


type PMap = Data.Array.Array VPage MFN

(!) :: (Show i, Data.Array.Ix i) => Data.Array.Array i e -> i -> e
(!) arr ix = let b = Data.Array.bounds arr
             in if Data.Array.inRange b ix
                then arr Data.Array.! ix
                else error ("Out of bounds: " ++ show ix ++
                              " is not in the range " ++ show b)

-- The level of a page map structure, 1 or 2 bits
type Level    = Int     
lvl2_num     :: Word
lvl3_num     :: Word

#ifdef CONFIG_X86_PAE
lvl2_num      = 4
lvl3_num      = 1
#else
lvl2_num      = 1
lvl3_num      = 0
#endif

stack_pages :: Word
stack_pages = 1 -- 128

-- Word as big as the machine word.
newtype VPage = VPage Word{-20/36-} deriving (Eq,Ord,Num,Bits,Enum,Data.Array.Ix)

instance Show VPage where
  show (VPage x) = show_hex x

type Index    = Word -- An index into a page map strucuture, 9 or 10 bits

incVPage :: Integral a => VPage -> a -> VPage
incVPage (VPage x) y = VPage (x+ fromIntegral y)

vpage_to_word :: VPage -> Word
vpage_to_word (VPage x) = x


-- | Number of bits used to index in a page map.
ix_bits :: Int
ix_bits = bytes_to_bits entries

-- | The number of entries in a page map.
entries :: Word
entries = entry_num x
  where x :: Page Entry
        x = error "to determine the number of entries in a page map"


--------------------------------------------------------------------------------

-- Virtual machines expect the mappings to be continuous.
-- Because of this, we fill gaps with RW pages.
-- The choice of RW is arbitrary, as far as I (ISD) know.
lvl1_entry :: PMap -> Map.Map VPage Access -> VPage -> Entry
lvl1_entry phys_map perm_map vaddr = entry mfn (access_flags access)
  where
  mfn     = phys_map ! vaddr
  access  = Map.findWithDefault Access_readWrite vaddr perm_map


-- The permissions on page map entries above level 1 can be used to
-- specify the permissions for a whole group of entries.
-- We do not use this feature, which is why we use RW here.
-- Instead, we specify the permissions for each page separately.
lvl2_entry :: PMap -> VPage -> Entry
lvl2_entry phys_map vaddr = entry mfn (access_flags Access_readWrite)
  where mfn = phys_map ! vaddr

-- Note that using 'readOnly' here is a bit of a hack: the RW bit
-- fir lvl3 page maps is reserved, so it should be 0, which conincides
-- with RO.
lvl3_entry :: PMap -> VPage -> Entry
lvl3_entry phys_map vaddr = entry mfn (access_flags Access_readOnly)
  where mfn = phys_map ! vaddr

-- | Write (contiguous) page tables for a contiguous region of virtual space,
-- using a 1-1 mapping from the given array.
write_lvl1  :: DomId -> PMap -> Map.Map VPage Access
            -> VPage      -- ^ Location of page map
            -> VPage      -- ^ First entry in page map
            -> Word       -- ^ How many page tables
            -> IO ()
write_lvl1 dom phys_map perm_map ptab_addr entry_addr num =
  zipWithM_ make_ptab ptab_addrs ptab_datas
  where
  make_ptab mfn es  = do save_page_map dom (mfn, zip [0..] es)
                         markAsPageTableMFN 1 mfn dom                         
  all_ents          = map (lvl1_entry phys_map perm_map) [ entry_addr .. ]
  ptab_datas        = chunks (fromIntegral entries) all_ents
  ptab_addrs        = map (phys_map !) $ genericTake num [ ptab_addr .. ]


write_lvl2 :: DomId -> PMap -> Map.Map VPage Access
           -> VPage      -- ^ Location of page dir
           -> VPage      -- ^ First entry in page maps
           -> Word       -- ^ How many page tables
           -> IO ()
write_lvl2 dom phys_map perms_map pdir_addr entry_addr num =
  do write_lvl1 dom phys_map perms_map ptab_addr entry_addr num
     zipWithM_ make_pdir pdir_addrs pdir_datas
  where
  make_pdir mfn es  = save_page_map dom (mfn, zip [0..] es)
  ptab_addr         = incVPage pdir_addr lvl2_num

  -- Number of blank spots at the beginning fo the first page directory
  init_blank  = vpage_to_word entry_addr `shiftR` ix_bits
  all_ents    = replicate (fromIntegral init_blank) unmapped ++
                genericTake num (map (lvl2_entry phys_map) [ ptab_addr .. ]) ++
                repeat unmapped
  pdir_datas  = chunks (fromIntegral entries) all_ents
  pdir_addrs  = map (phys_map !) $ genericTake lvl2_num [ pdir_addr .. ]


write_lvl3 :: DomId -> PMap -> Map.Map VPage Access
           -> VPage      -- ^ Location of page dir ptr
           -> VPage      -- ^ First entry in page maps
           -> Word       -- ^ Number of page tables
           -> IO ()
write_lvl3 dom phys_map perms_map pdir_ptr_addr entry_addr num
  | lvl3_num /= 0 =
  do write_lvl2 dom phys_map perms_map pdir_addr entry_addr num
     save_page_map dom (phys_map ! pdir_ptr_addr, ents)
     markAsPageTableMFN 3 (phys_map ! pdir_ptr_addr) dom
  where
  pdir_addr = pdir_ptr_addr + 1
  ents      = zip [0 .. lvl2_num - 1]
                  (map (lvl3_entry phys_map) [ pdir_addr .. ])

-- No 3rd level:
write_lvl3 dom phys_map perms_map pdir_ptr_addr entry_addr num =
  do write_lvl2 dom phys_map perms_map pdir_ptr_addr entry_addr num
     markAsPageTableMFN 2 (phys_map ! pdir_ptr_addr) dom
     
-- | Saves the page maps for the given domain.
-- Assummes that the page map is non-empty.
write_pmaps :: DomId -> VPage -> PMap -> Map.Map VPage Access
            -> IO (VPage, Word)
write_pmaps dom first_addr phys_map perms_map =
  do let it = vpage_to_word first_addr
  {-
     writeDebugConsole ("Virtual space: " ++ show_hex it ++
                " - " ++ show_hex (it + fromIntegral lvl1_num * entries - 1) ++ "\n")
                -}
     write_lvl3 dom phys_map new_perms pmap_addr first_addr lvl1_num
     return (pmap_addr, pmap_pages)
  where

  -- first_addr      = fst $ Map.findMin perms_map
  last_addr       = fst $ Map.findMax perms_map
  pmap_addr       = last_addr + 1

  pmap_pages  = lvl1_num + lvl2_num + lvl3_num
  user_pages  = vpage_to_word (last_addr - first_addr + 1)

  -- The extra 512K end up after the mappings for the page maps.
  -- (see the comment on start_info in public/xen.h)
  extra_space = round_up (512 * 1024) (fromIntegral pageSize)

  -- We also allocate some pages for a bootstrap stack.
  -- Not sure who uses that, but it is mentioned in public/xen.h

  -- lvl1_num should be the solution to these two equations:
  --   total    = user_pages + lvl1_num + lvl2_num + lvl3_num + extra_space
  --   lvl1_num = round_up total entries
  lvl1_num    = round_up (user_pages + lvl2_num + lvl3_num + stack_pages
                                                           + extra_space)
                         (entries - 1)

  -- Page maps should be mapped as read-only, so we add extra entries for them.
  new_perms   = foldl' (\m a -> Map.insert a Access_readOnly m) perms_map
              $ genericTake pmap_pages [pmap_addr .. ]


-- | How to initialize a page map. Maps indexes to entries.
type PMapData = [(Index,Entry)]

-- | Save a page table.
save_page_map :: DomId -> (MFN,PMapData) -> IO ()
save_page_map dom (mfn,ents) = with_page dom mfn $ \p ->
  do set_entries p ents
     setPageWritable (unsafe_ptr p) False (Just dom)

dump_pmap :: MFN -> Level -> PMapData -> IO ()
dump_pmap mfn lvl xs =
  do writeDebugConsole $ "-- Page map, mfn = " ++ show_hex (fromMFN mfn)
                      ++ ", level = " ++ show lvl ++ " -----\n"
     mapM_ sh (filter yes xs)
  where sh (x,y)  = writeDebugConsole (show_hex x ++ '\t' : show y ++ "\n")
        yes (_,p) = get_flag p present


