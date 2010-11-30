-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module DomainBuilder.LoadElf(loadElf,elfInfo) where

import DomainBuilder.AllocM
import DomainBuilder.IfaceTypes(Access(..))
import DomainBuilder.PageMap
import DomainBuilder.Utils
import qualified Elf.Elf as Elf
import qualified Elf.Sum_Type as Sum
import Hypervisor.Memory(pageSize)

import qualified Data.ByteString as BS
import Data.Word

--import Hypervisor.Debug

-- REVISIT - better error reporting
-- REVISIT - there are too many uses of 'fromIntegral' here---make sure that
--           all these casts are OK.

-- | Load an ELF image from a static image.
-- Note: the image should be an uncompressed image, as the loader cannot
-- handle compressed images, currently.
-- Returns:
--   * the section headers from the elf file
--   * the page number for the virtual base address
--   * the virtual address for the entry point of the image
--   * the virtual page_number for the hyper-call page
loadElf :: BS.ByteString -> [Elf32_phdr] -> AllocM ()
loadElf elfImage phdrs = mapM_ (loadPhdr elfImage) phdrs

elfInfo :: BS.ByteString -> ([Elf32_phdr],VPage,Integer,VPage)
elfInfo elfImage =
  let (Sum.Inr (ehdr, (phdrs, (_, xennotes))), _) =
                            Elf.runEm Elf.parseEPShdrsXen (emOps elfImage)
  in ( phdrs
     , fromIntegral (Elf.xen_elfnote_virt_base xennotes `div` pg)
     , Elf.e_entry ehdr
     , fromIntegral (Elf.xen_elfnote_hypercall_page xennotes `div` pg)
     )
  where pg = fromIntegral pageSize


emOps :: BS.ByteString -> Elf.EmOps_ext_type BS.ByteString ()
emOps s = Elf.Abs_EmOps_ext_type
          (   initial
          , ( next_char
          , ( seek_abs
          , ( seek_rel
          , ( seek_chunk
          , ())))))
  where initial = s
        next_char x | BS.null x = Nothing
                    | otherwise = Just (toEnum (fromIntegral (BS.head x))
                                       ,BS.tail x)
        seek_abs n       = BS.drop (fromIntegral n) s
        seek_rel n x     = BS.drop (fromIntegral n) x
        seek_chunk n sz =  BS.take (fromIntegral sz)
                             (BS.drop (fromIntegral n) s)

type Elf32_phdr = Elf.Elf32_phdr_ext_type ()

loadPhdr :: BS.ByteString -> Elf32_phdr -> AllocM ()
loadPhdr elfImage phdr
 = case Elf.p_type phdr of
     0 -> return ()
     1 -> -- PT_LOAD
          if pageSize `mod` fromIntegral (Elf.p_align phdr) == 0
            then do let start = fromIntegral (Elf.p_vaddr phdr `div` fromIntegral pageSize)
                    let memSize = Elf.p_memsz phdr
                    loadImagePages
                      (elfAccess (Elf.p_flags phdr))
                      (BS.drop (fromIntegral (Elf.p_offset phdr)) elfImage)
                      (fromIntegral (Elf.p_filesz phdr))
                      (fromIntegral memSize)
                      start
            else error "page alignment must suffice for ELF sections"
     2 -> error "PT_DYNAMIC ELF section unsupported.  Try static linking."
     3 -> error "PT_INTERP ELF section unsupported.  Try a compiler."
     4 -> return () -- PT_NOTE ignored
     5 -> error ("PT_SHLIB ELF section reserved by ELF specification "
                 ++"and hence unspported")
     6 -> error "PT_PHDR ELF section unsupported" -- REVISIT treat like load?
     _ -> return () -- error "undocumented ELF section"



loadImagePages :: Access           -- ^ Permissions for the mapping.
               -> BS.ByteString    -- ^ Data for initialization.
               -> Word             -- ^ Initialized data, in bytes.
               -> Word             -- ^ Whole image size, in bytes.
               -> VPage            -- ^ Virtual page number to start from.
               -> AllocM ()
loadImagePages access sectionData fileSize memSize vaddr =
  do debug $ "Section " ++ show vaddr ++ ", " ++ show initPages
           ++ " init. pages, " ++ show zeroPages ++ " zero pages.\n"
     alloc_string vaddr access (BS.take (fromIntegral initBytes) sectionData)
     mapM_ (`alloc_zero` access) $
          take (fromIntegral zeroPages) [ incVPage vaddr initPages .. ]

  where initBytes = min fileSize memSize
        initPages = fromIntegral (round_up initBytes pg)
        zeroPages = round_up (memSize - initPages * pg) pg
        pg        = fromIntegral pageSize

debug :: String -> AllocM ()
debug _ = return ()
--debug x = io $ writeDebugConsole ("[ELF] " ++ x)


{-
-- to find the memory management unit access control settings for a given
-- set of ELF program header flags
-- ELF flags are 4, 2, and 1 for read, write, and execute respectively.
elfAccess :: Integer -> I.Access
elfAccess i
 = if testBit i 2
     then if testBit i 1
            then I.Access_readWrite
            else I.Access_readOnly
     else I.Access_none
-}
-- FIX - halvm's stack is in the text segment (sigh), so just make all the
-- pages writable
-- FIX - Adam supposedly moved HALVM's stack to the data segment, so try
-- using read-only pages again.
elfAccess :: Integer -> Access
elfAccess _ = Access_readWrite


