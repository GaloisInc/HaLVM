-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>, Iavor S. Diatchki <diatchki@galois.com>
-- BANNEREND
--
module DomainBuilder.CreateVM
  ( DomainHandle
  , MemMap(..)
  , domainId
  , withStartPage
  , createVm
  ) where

import qualified DomainBuilder.SafePage as Sp
import qualified DomainBuilder.StartPage as StartPage
import DomainBuilder.AllocM
import DomainBuilder.LoadElf
import DomainBuilder.SafePage1
import DomainBuilder.PageMap
import DomainBuilder.IfaceTypes(Access(..))
import DomainBuilder.Registers
import DomainBuilder.Utils

import qualified Hypervisor.Privileged as Priv
import Hypervisor.Basics
import Hypervisor.Memory

import Control.Monad(when,zipWithM_)
import qualified Data.ByteString as BS
import Data.List
import Data.Word
import Foreign.Storable(sizeOf)


-- | An opaque structure defining a new domain.
data DomainHandle = DomainHandle { domId :: DomId, domStartPageMfn :: MFN }

-- | Given a DomainHandle, return the domain id of the new domain.
domainId :: DomainHandle -> DomId
domainId = domId

-- | Perform some modifications or other operations for the domain given
-- the start page.
withStartPage :: DomainHandle -> (Sp.Page -> IO a) -> IO a
withStartPage h m = withStartPageArr h $ \p -> m (Sp.mkPage (unsafe_ptr p))

withStartPageArr :: DomainHandle -> (Page b -> IO a) -> IO a
withStartPageArr h m = with_page (domId h) (domStartPageMfn h) m



-- | Interesting areas in virtual space.
data MemMap = MemMap
  { kernel          :: (VPage,VPage)      -- ^ First-last page
  , ram_disk        :: Maybe (VPage,Word)  -- ^ Start, number of bytes
  , blank_pages     :: (VPage, [MFN])     -- ^ First page, MFNs for pages
  , hyper_page      :: VPage
  , phys_mach_map   :: (VPage,Word)       -- ^ First page, number of entries
  , start_info_page :: VPage
  , pmaps           :: (VPage,Word)       -- ^ Start, number of pages
  }

instance Show MemMap where
  show m = unlines
    [ "Memory map:"
    , "  kernel:      " ++ show (kernel m)
    , "  hyper page:  " ++ show (hyper_page m)
    , "  ram disk:    " ++ show (ram_disk m)
    , "  blank pages: " ++ show (blank_pages m)
    , "  phys-mach:   " ++ show (phys_mach_map m)
    , "  start info:  " ++ show (start_info_page m)
    , "  page maps:   " ++ show (pmaps m)
    ]

createVm :: Maybe DomId         -- ^ Optional domain id
         -> SID                 -- ^ Security identifier for child
         -> Word64              -- ^ Requested memory
         -> BS.ByteString       -- ^ ELF image
         -> Maybe BS.ByteString -- ^ Optional RAM disk
         -> [Access]            -- ^ Extra blank pages
         -> IO (DomainHandle, MemMap)
createVm childid sid maxMemory elfImage initrd blank =
  do -- Make a new domain
     domid <- Priv.createDomain childid sid
                  $ map (fromIntegral . fromEnum) "SivcSivcSivcSivc"
     Priv.setDomainMaxVCPUs domid 1 -- default, but set it anyway.

     -- Allocate some real memory
     Priv.setDomainMaxMemory domid maxMemory
     ms <- Priv.allocForeignMachineFrames domid (fromIntegral maxMemory)
     let mfns = map toMFN ms

     -- Setup the child's data
     (dom_h, mem_map, entry, ptab_mfn) <-
                                  setup_mem domid mfns elfImage initrd blank
     initStartInfoPage dom_h mem_map

     let mm = pmaps mem_map
         stack = fromIntegral
                    (vpage_to_word (incVPage (fst mm) (snd mm + stack_pages)))
                      * fromIntegral pageSize
     initRegs domid entry ptab_mfn (start_info_page mem_map) stack


     return (dom_h, mem_map)


setup_mem :: DomId                -- ^ Child's id
          -> [MFN]                -- ^ Available machine frames
          -> BS.ByteString        -- ^ ELF image
          -> Maybe BS.ByteString  -- ^ Optional RAM disk
          -> [Access]             -- ^ Permissions for extra blank pages
          -> IO ( DomainHandle
                , MemMap
                , Integer         -- Entry point
                , MFN             -- MFN of top page map (for CR3)
                )
setup_mem domid mfns elfImage initrd blank =
  do let (elf_headers,virt_base,entry,hypercalls) = elfInfo elfImage

     ((dom_h,mem_map), ptab_start, ptab_mfn, ptab_num) <-


       -- run_alloc takes care of allocating and mapping the page maps
       -- into the child.
       run_alloc domid virt_base mfns $
       do -- Load the kernel data from the ELF image
          loadElf elfImage elf_headers

          -- Map the interface to the hypervisor into the kernel.
          hypercalls_mfn <- lookup_mfn hypercalls
          io $ Priv.hypercallInit domid $ fromMFN hypercalls_mfn

          (kernel_start,kernel_end) <- vpage_range

          -- Load the RAM disk, if any
          mb_ram <- case initrd of
                      Nothing -> return Nothing
                      Just bs ->
                        do let start = kernel_end + 1
                           alloc_string start Access_readWrite bs
                           return $ Just (start,fromIntegral (BS.length bs))

          -- Allocate some blank pages for programmer use
          let blank_start =
                maybe (kernel_end + 1)
                      (\(s,n) -> incVPage s
                                  (round_up n (fromIntegral pageSize))) mb_ram
              blank_num   = length blank
          zipWithM_ alloc_zero [blank_start..] blank
          blank_mfns <- mapM lookup_mfn $ take blank_num [blank_start..]

          -- Initialize the physical to machine map
          let phys_map_start = incVPage blank_start blank_num
          -- NOTE: we could use the MFN array in the env. to avoid holding
          -- on to the list.
          -- NOTE: The HALVM modifies the entries in the phys_map
          -- (it uses a bit to mark what pages are used).
          -- This is why we map the phys-to-machine-map as RW.
          alloc_phys_map phys_map_start mfns
          phys_page_num <- mfn_num
          phys_map_end <- last_vpage

          -- Allocate the start info page
          let start_v = phys_map_end + 1
          alloc_zero start_v Access_readWrite
          start_m <- lookup_mfn start_v

          return ( DomainHandle { domId = domid
                                , domStartPageMfn = start_m
                                }

                 , MemMap { kernel          = (kernel_start, kernel_end)
                          , ram_disk        = mb_ram
                          , blank_pages     = (blank_start,blank_mfns)
                          , hyper_page      = hypercalls
                          , phys_mach_map   = (phys_map_start, phys_page_num)
                          , start_info_page = start_v
                          , pmaps           = (0,0) -- Unknown, see bellow
                          }
                 )

     return (dom_h, mem_map { pmaps = (ptab_start, ptab_num) }, entry, ptab_mfn)


-- | Save a list of MFNs.
alloc_phys_map :: VPage -> [MFN] -> AllocM ()
alloc_phys_map addr mfns = zipWithM_ save_chunk [addr .. ]
                                          (pad_last (chunks per_page mfns))
  where per_page        = fromIntegral pageSize `div` sizeOf (head mfns)
        access          = Access_readWrite
        save_chunk v ws = alloc_page v access (`set_entries` zip [0..] ws)

        pad_last []     = []
        pad_last [xs]   = [xs ++ replicate (per_page - length xs) 0]
        pad_last (x:xs) = x : pad_last xs





-- Initialize the start info page.
-- We only fill in the information required by a HaLVM (plus ramdisk info).
initStartInfoPage :: DomainHandle -> MemMap -> IO ()
initStartInfoPage h m = withStartPage h $ \page ->
  do let set :: Sp.Offset -> Word -> IO ()
         set o a = do ok <- Sp.setElem page o a
                      when (not ok) $
                        fail ("Failed to initialize start info @ " ++ show o)

     info <- Priv.domainInfo (domainId h)
     set StartPage.shared_info
          $ fromIntegral (Priv.shared_info_frame info * pageSize)

     let (phys_start,phys_num) = phys_mach_map m
     set StartPage.mfn_list (vpage_to_addr phys_start)
     set StartPage.nr_pages phys_num

     let (ptab_start, ptab_num) = pmaps m
     set StartPage.pt_base (vpage_to_addr ptab_start)
     set StartPage.nr_pt_frames ptab_num

     case ram_disk m of
       Just (modStart,modLen) ->
         do set StartPage.mod_start (vpage_to_addr modStart)
            set StartPage.mod_len modLen
       Nothing -> return ()

  where -- Assumes that words are at least as big as virtual addresses.
        vpage_to_addr :: VPage -> Word
        vpage_to_addr (VPage v) = v * fromIntegral pageSize



