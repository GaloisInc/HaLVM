{-# OPTIONS -fglasgow-exts -cpp #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |A low-level module for dealing with unprivileged Xen memory operations,
-- including allocating pages, granting access to pages to other domains, and
-- mapping the grants of other domains.
module Hypervisor.Memory(
         -- * Types and conversions for dealing with memory.
           PFN, MFN
         , VPtr, MPtr
         , mfnToMPtr, mptrToMFN , mptrToPtr, toMFN, fromMFN
         , mfnToVPtr, vptrToMFN
         , mptrToInteger
         , pageSize
         -- * Routines for creating, destroying, and modifying pages.
         , allocPage
         , AllocProt(..), defaultProt
         , allocPageProt
         , freePage
         , withPage
         , setPageWritable
         , markAsPageTable
         , markAsPageTableMFN
         -- * Routines for creating or destroying grant references 
         -- and grant handles.
         , GrantRef(..)
         , allocRef
         , freeRef
         , withRef
         , grantAccess
         , endAccess
         , initiateGrants
         , grantRefToAddress
         , GrantHandle(..)
         , mapGrant
         , mapGrants
         , unmapGrant
         -- * Routines for transferring or copying pages to another domain.
         , grantForeignTransferRef 
         , finishForeignTransferRef
         , resetForeignTransferRef
         , transferPageToForeignDomain
         , performFrameCopy
         -- * Low-level routines for dealing with frames, address translation,
         -- and similar grungy things.
         , translateForeignAddress
         , mapForeignMachineFramesReadOnly
         , mapForeignMachineFrames
         , unmapForeignMachineFrames
         , virtualToMachine
         , machineToVirtual
         , reserveVirtualFrames 
         , unreserveVirtualFrames
         , addressMapped
         , systemWMB, systemRMB, systemMB
         )
    where

import Control.Monad
import Data.Bits
import Data.Int
import Data.IORef
import Data.Word
import Foreign.Ptr
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.IO(unsafePerformIO)
import Hypervisor.Basics
#if !defined(CONFIG_X86_64)
import Hypervisor.Privileged
#endif
import Data.Generics.Instances()
import Data.Generics.Basics(Data,Typeable)
import Numeric

--
-- * Types and conversions for dealing with memory.
--

-- |Pseudo-physical frame numbers. These frame numbers have very little to
-- do with the machine address or the virtual address, but are used in some
-- Xen hypercalls.
newtype PFN = PFN Word

_ignore :: Int
_ignore = undefined PFN

-- |Machine frame numbers. These frame numbers identify a phyical 4096-byte
-- frame on the underlying hardware.
newtype MFN = MFN Word
 deriving (Eq, Ord, Num, Read, Data, Typeable)

instance Show MFN where
  show (MFN x) = "MFN 0x" ++ showHex x ""

-- NOTE: we could derive this
instance Bits MFN where
  (MFN x)  .&.  (MFN y)     = MFN $ (x .&. y)
  (MFN x)  .|.  (MFN y)     = MFN $ (x .|. y)
  (MFN x) `xor` (MFN y)     = MFN $ (x `xor` y)
  (MFN x) `shift` y         = MFN $ x `shift` y
  (MFN x) `rotate` y        = MFN $ x `rotate` y
  (MFN x) `setBit` y        = MFN $ setBit x y
  (MFN x) `clearBit` y      = MFN $ clearBit x y
  (MFN x) `complementBit` y = MFN $ complementBit x y
  complement (MFN x)        = MFN $ complement x
  bit x                     = MFN $ bit x
  (MFN x) `testBit` y       = testBit x y
  bitSize _                 = bitSize (0 :: Word)
  isSigned _                = False

-- NOTE: we could derive this
instance Storable MFN where
  sizeOf _         = sizeOf (0 :: Word)
  alignment _      = alignment (0 :: Word)
  peek ptr         = MFN `fmap` peek (castPtr ptr)
  poke ptr (MFN v) = poke (castPtr ptr) v


-- |A virtual address that, if you've mapped it, can be written to and read
-- from as per normal.
type VPtr a = Ptr a

-- |A machine address. These cannot be written to or read from directly, as
-- HaLVM's always run with paging enabled.
#if defined(CONFIG_X86_PAE) || defined(CONFIG_X86_64)
newtype MPtr a = MPtr Word64 deriving Storable
#else
newtype MPtr a = MPtr Word32 deriving Storable
#endif

mptrToInteger :: MPtr a -> Integer
mptrToInteger (MPtr x) = fromIntegral x

-- |Convert a 32-bit word, from some other source, into an MFN. Manufacturing
-- your own MFNs is dangerous, so make sure you know what you're doing if 
-- you use this function.
toMFN :: Word -> MFN
toMFN = MFN

-- | This is used when passing MFNs to some primitives.
-- Eventually, we should change the primitives to take MFNs directly.
fromMFN :: MFN -> Word
fromMFN (MFN x) = x

-- |Convert a machine frame number to the initial machine address within the
-- block.
mfnToMPtr :: MFN -> MPtr a
mfnToMPtr (MFN f) = MPtr (fromIntegral f `shiftL` 12)

-- |Convert a machine frame number to the address at which it is mapped in
-- the address space. Note that, obviously, if the page isn't currently 
-- mapped, you'll get an error.
mfnToVPtr :: MFN -> Xen (VPtr a)
mfnToVPtr = machineToVirtual . mfnToMPtr

-- |Convert a virtual address to the machine frame underlying its frame. This
-- function will perform the rounding for you. If the page is mapped (if
-- addressMapped) returns True, then this page is guaranteed to succeed.
vptrToMFN :: VPtr a -> Xen MFN
vptrToMFN x = do
  p <- virtualToMachine x
  return (mptrToMFN p)

-- |Convert a machine pointer to its machine frame number. This operation
-- is necessarily lossy, so (x == mptrToMFN (mfnToMPtr x)) does not 
-- necessarily hold.
mptrToMFN :: MPtr a -> MFN
mptrToMFN (MPtr p) = fromIntegral (p `shiftR` 12)

-- |Convert a machine pointer to a pointer. In order to use this, you should
-- really know what you're doing. Reading to or from the returned address
-- will probably cause a crash.
mptrToPtr :: MPtr a -> Ptr a
mptrToPtr (MPtr p) = intPtrToPtr (fromIntegral p)

-- |The size, in bytes, of a memory page. 
pageSize :: Word32
pageSize = 4096

--
-- * Routines for creating, destroying, and modifying pages.
--


-- |Allocate a page outside the garbage-collected heap. These pages
-- are almost always used with grants.
allocPage :: Xen (VPtr a)
allocPage = do
  va <- alloc_page
  if va == nullPtr then xThrow ENOMEM else return $! va

data AllocProt = AllocProt
  { protRead    :: Bool
  , protWrite   :: Bool
  , protExec    :: Bool
  , protNoCache :: Bool
  }

-- | These are the Prot flags used by allocPage
defaultProt :: AllocProt
defaultProt  = AllocProt
  { protRead    = True
  , protWrite   = True
  , protExec    = True
  , protNoCache = False
  }

getProt :: AllocProt -> Int
getProt flags =  flag (bit 0) protRead
             .|. flag (bit 1) protWrite
             .|. flag (bit 2) protExec
             .|. flag (bit 3) protNoCache
  where
  flag b p | p flags   = b
           | otherwise = 0

-- | Allocate with a set of protection flags.
allocPageProt :: AllocProt -> Xen (VPtr a)
allocPageProt flags = do
  va <- mmap nullPtr 0x1000 (getProt flags) 0 (-1) 0
  if va == nullPtr then xThrow ENOMEM else return $! va

-- |Free a page allocated with allocPage.
freePage :: VPtr a -> Xen ()
freePage x 
  | x /= (x `alignPtr` 4096) = xThrow EINVAL
  | otherwise                = 
      setPageWritable x True Nothing >> free_page x >> return ()

-- | Allocate a page, call a function with it, and free it.  Return the result
--   in the Xen monad, capturing the possibility of a page allocation failure.
withPage :: (VPtr a -> IO b) -> Xen b
withPage f = allocPage >>= \page -> do
    b <- f page
    freePage page
    return b

-- |Set a page writable (or not).
setPageWritable :: VPtr a -> Bool -> Maybe DomId -> Xen ()
setPageWritable x val Nothing = setPageWritable x val (Just domidSelf)
setPageWritable x val (Just (DomId dom)) = do
  res <- set_page_writable x val dom
  if res == 0
    then return ()
    else xThrow (toEnum (fromIntegral (-res)))

-- |Mark the given page as one that will be used as a page table.
-- The given address is a virtual address. This is the analagous
-- version of the MMUEXT_PIN_L?_TABLE case of the MMUext hypercall;
-- the argument specifying what level. 
-- QUICK GUIDE:
--   Use level '1' for page tables
--   Use level '2' for page directories
--   Use level '3' for PAE base tables
markAsPageTable :: Int -> VPtr a -> DomId -> Xen ()
markAsPageTable l addr (DomId dom) 
  | (l >= 1) && (l <= 4) = do
     res <- mark_as_page_table l addr dom
     case res of
       0 -> return ()
       _ -> xThrow (toEnum (fromIntegral (-res)))
  | otherwise = 
     xThrow EINVAL

markAsPageTableMFN :: Int -> MFN -> DomId -> Xen ()
markAsPageTableMFN l mfn (DomId dom) 
  | (l >= 1) && (l <= 4) = do
     res <- mark_as_page_table_mfn l mfn dom
     case res of
       0 -> return ()
       _ -> xThrow (toEnum (fromIntegral (-res)))
  | otherwise = 
     xThrow EINVAL



--
-- * Routines for creating or destroying grant references and grant handles.
--

newtype GrantRef = GrantRef Word16
 deriving (Eq, Ord, Show, Read, Typeable, Data)

num_grant_entries :: Num a => a
num_grant_entries = 2048

{-# NOINLINE unusedGrantRefs #-}
unusedGrantRefs :: IORef [Word16] 
unusedGrantRefs = unsafePerformIO $ newIORef [8..num_grant_entries-1]

-- |Allocate a grant reference that may later be used to grant access to
-- a page to another domain.
allocRef :: Xen GrantRef
allocRef = do
  mgr <- atomicModifyIORef unusedGrantRefs getr 
  case mgr of
    Left e   -> xThrow e
    Right gr -> return gr
 where getr []     = ([], Left ENOMEM)
       getr (r:rs) = (rs, Right $ GrantRef r)

-- |Free an allocated grant reference.
freeRef :: GrantRef -> IO ()
freeRef (GrantRef r) = atomicModifyIORef unusedGrantRefs (\ rs -> (r:rs,()))

-- | Allocate a ref, run a function with it, and free it.  Return the result in
--   the Xen monad, to capture a possible failure.
--
--   Note: all access to the ref is ended, via endAccess, before freeRef is
--   called.
withRef :: (GrantRef -> IO b) -> Xen b
withRef f = allocRef >>= \ref -> do
    b <- f ref
    endAccess ref
    freeRef   ref
    return b

-- |Grant access to the page to given domain, using the given grant reference.
-- The final, boolean argument determines whether or not the given domain
-- can write to the page (True) or not (False).
grantAccess :: GrantRef -> DomId -> VPtr a -> Bool -> IO ()
grantAccess (GrantRef gr) (DomId remoteDomain) vaddr writable = 
  gnttab_grant_access gr remoteDomain vaddr writable

-- |Stop any access grants associated with the given grant reference.
endAccess :: GrantRef -> Xen ()
endAccess (GrantRef gr) = do
  res <- gnttab_end_access gr
  if res
     then return ()
     else xThrow EBUSY 

-- |Creates a new page and then allows the given domain to access the
-- page. The other domain may write to the page if the second argument
-- is true. This function is largely shorthand for the straightforward
-- combination of allocPage, allocRef, grantAccess, and ignores any
-- errors.
initiateGrants :: DomId -> Int -> Bool -> IO (VPtr a, [GrantRef])
initiateGrants remoteDomain s writable = do
  -- BUG: This will leak memory off the beginning or end of the
  -- allocated amount.
  badptr <- mallocBytes $ 4096 + (4096 * s)
  refs   <- replicateM s (ignoreErrors allocRef)
  let ptr = alignPtr badptr 4096
  grantBufferAccess ptr refs
  return (ptr, refs)
 where
  grantBufferAccess _   [] = return ()
  grantBufferAccess ptr (ref:rest) = do
    grantAccess ref remoteDomain ptr writable
    grantBufferAccess (ptr `plusPtr` 4096) rest

-- |Given a grant reference, find the virtual address associated with that
-- reference.
grantRefToAddress :: GrantRef -> Xen (VPtr a)
grantRefToAddress (GrantRef gr) = do
  res <- gnttab_address_of gr
  if res /= nullPtr then return res else xThrow EINVAL
   
-- |The type of a grant handle, or (in other words), the handle to a
-- grant from another domain that we've mapped.
newtype GrantHandle = GrantHandle Word32
  deriving (Eq, Ord, Show, Read)	

-- |Map another domain's grant into our own address space. The return
-- values, if successful, are a pointer to the newly-mapped page in 
-- memory and a grant handle. The boolean argument determines whether
-- HALVM should map the page read-only (False) or read\/write (True).
mapGrant :: DomId -> GrantRef -> Bool -> Xen (VPtr a, GrantHandle)
mapGrant (DomId remoteDomain) (GrantRef grantRef) writable = do
  vaddr <- claim_vspace nullPtr 4096
  if (vaddr == nullPtr)
     then xThrow ENOMEM
     else mapGrant' vaddr dom gref writable
 where
  dom  = fromIntegral remoteDomain
  gref = fromIntegral grantRef

-- |Map a set of grant references from another machine into our own
-- address space in linear order. The first grant listed will be
-- the start of the return pointer, the second will be that with
-- a 4096-byte offset, etc..
mapGrants :: DomId -> [GrantRef] -> Bool -> Xen (VPtr a, [GrantHandle])
mapGrants _ [] _ = xThrow EINVAL
mapGrants (DomId remoteDomain) refs writable = do
  vaddr <- claim_vspace nullPtr $ fromIntegral size
  if (vaddr == nullPtr)
    then xThrow ENOMEM
    else do hndls <- mapAllGrants vaddr refs `xOnException`
                        disclaim_vspace vaddr (vaddr `plusPtr` size)
            return $! (vaddr, hndls)
 where
  size = 4096 * length refs
  dom  = fromIntegral remoteDomain
  mapAllGrants _ [] = return []
  mapAllGrants ptr ((GrantRef gref):rest) = do
    rest' <- mapAllGrants (ptr `plusPtr` 4096) rest
    (_, hndl) <- mapGrant' ptr dom (fromIntegral gref) writable `xOnException`
                     mapM_ (`unmapGrant` Nothing) rest'
    return $! (hndl:rest')

mapGrant' :: VPtr a -> Word16 -> Word32 -> Bool -> Xen (VPtr a, GrantHandle)
mapGrant' vaddr dom gref writable = do
  res <- gnttab_map_grant_ref vaddr dom gref writable
  case res of
    x | x < 0    ->
         xThrow $ toEnum $ fromIntegral (-x)
      | x < num_grant_entries ->
         return $ (vaddr, GrantHandle $ fromIntegral x)
      | otherwise ->
         xThrow EOK -- error "INTERNAL ERROR: Bad grant handle! (mapGrant)"


-- |Unmap the grant of another domain's page. Optionally, (if the 
-- second argument is not Nothing), it will remove the associated
-- page from the address space. You will almost always want to do
-- this.
unmapGrant :: GrantHandle -> Maybe (VPtr a) -> Xen ()
unmapGrant _ (Just vaddr) | vaddr /= (vaddr `alignPtr` 4096) =
  xThrow EINVAL
unmapGrant (GrantHandle gh) _vaddr = do
  res <- gnttab_unmap_grant_ref (maybe nullPtr id _vaddr) gh
  case res of
    0 -> do case _vaddr of
              Just vaddr -> disclaim_vspace vaddr (vaddr `plusPtr` 4096)
              Nothing    -> return ()
            return ()
    _ -> xThrow $ toEnum $ fromIntegral (-res)

--
-- * Routines for transferring or copying pages to another domain.
--

-- |Allow the given foreign domain to transfer a page into the given
-- grant reference.
grantForeignTransferRef :: GrantRef -> DomId -> Xen ()
grantForeignTransferRef (GrantRef gr) (DomId dom) = do
  res <- gnttab_grant_foreign_transfer_ref gr dom
  case res of
    x | x == 0    -> return $ ()
      | otherwise -> xThrow $ toEnum $ fromIntegral (-x)

-- |Finish the foreign transfer, creating the page for the transferred
-- data, if successful. Note that in some cases this routine can fail
-- when called too early (in particular, before Xen notifies the domain
-- that the sending domain has started the send). So if that can happen,
-- you may want to add a delay.
finishForeignTransferRef :: GrantRef -> Xen (VPtr a)
finishForeignTransferRef (GrantRef gr) = do
  addr <- gnttab_finish_foreign_transfer_ref gr
  if addr == nullPtr
     then xThrow ENOMEM
     else return addr

-- |Reset the foreign transfer reference to its original state, allowing
-- it to accept further transfers. Note that this invalidates any page
-- returned via finishForeignTransferRef.
resetForeignTransferRef :: GrantRef -> Xen ()
resetForeignTransferRef (GrantRef gr) = do
  res <- gnttab_reset_foreign_transfer_ref gr
  case res of
    x | x == 0    -> return $ ()
      | otherwise -> xThrow $ toEnum $ fromIntegral (-x)

-- |Transfer a page to a domain given the grant reference it offered.
-- This reference must have been created using grantForeignTransferRef
-- (or the equivalent for other systems) on the other side; your standard,
-- run of the mill grant reference won't work.
transferPageToForeignDomain :: VPtr a -> DomId -> GrantRef -> Xen ()
transferPageToForeignDomain ptr (DomId dom) (GrantRef gr) = do
  res <- transfer_page_to_dom ptr dom gr
  case res of
    x | x == 0    -> return $ ()
      | otherwise -> xThrow $ toEnum $ fromIntegral (-x)

-- |Perform a copy of one frame to another frame. If two frame numbers are
-- used, they must be legitimate frame numbers for the calling domain. For
-- use between domains, the function can use grant references, which must 
-- be set as read/write for the appropriate domains. The first mfn/ref and
-- domain is the source, the second set is the destination. Note that it is
-- an error to specify an MFN with any other identifier than domidSelf.
performFrameCopy :: (Either GrantRef MFN) -> DomId -> Word16 ->
                    (Either GrantRef MFN) -> DomId -> Word16 ->
                    Word16 -> 
                    Xen ()
performFrameCopy src sd soff dest dd doff len = do
  let (snum,sisref) = argToVals src sd
      (dnum,disref) = argToVals dest dd
  ret <- perform_grant_copy snum sisref srcDom soff dnum disref destDom doff len
  case ret of
    x | x == 0    -> return $ ()
      | otherwise -> xThrow $ toEnum $ fromIntegral (-x)
 where 
  (DomId srcDom) = sd
  (DomId destDom) = dd
  argToVals :: (Either GrantRef MFN) -> DomId -> (Word32,Word32)
  argToVals (Left (GrantRef ref)) _ = (fromIntegral ref, 1)
  argToVals (Right (MFN _)) dom | dom /= domidSelf =
    error "Called with an MFN and non-self domain!"
  argToVals (Right (MFN mfn)) _ = (fromIntegral mfn, 0)

foreign import ccall unsafe "gnttab.h gnttab_grant_copy" 
  perform_grant_copy :: Word32 -> Word32 -> Word16 -> Word16 ->
                        Word32 -> Word32 -> Word16 -> Word16 ->
                        Word16 -> IO Int32

--
-- * Low-level routines for dealing with frames, address translation,
-- and similar grungy things.
--

-- !Lookup the MFN that a given address is mapped to in a foreign
-- domain's address-space.
#if defined(CONFIG_X86_64)
translateForeignAddress :: DomId -> VCPU -> Ptr a -> Xen MFN
translateForeignAddress _did _vcpu _remAddr = do
  fail "translateForeignAddress: not implemented"
{-
  ctxt <- domainRegisterContext did vcpu
  let l4 = fromIntegral (cr3 (ctrlreg ctxt))
  l3 <- lookupPT l4 (ptIndex 4)
  l2 <- lookupPT l3 (ptIndex 3)
  l1 <- lookupPT l2 (ptIndex 2)
  mfn <- lookupPT l1 (ptIndex 1)

  return (MFN mfn)


  where
  ra :: Word
  ra = fromIntegral $ ptrToIntPtr remAddr

  ptIndex l = ra `shiftR` (3 + l * 9)

  lookupPT maddr i = do
    pt <- mapForeignMachineFramesReadOnly did [MFN (0xFFFFFFFFF .&. (maddr `shiftR` 12))]
    pte <- readPageEnt pt i
    unmapForeignMachineFrames pt 4096
    if (pte .&. 1) == 0
      then xThrow EINVAL
      else return (pte `shiftR` 12)

  readPageEnt :: VPtr a -> Int -> IO Word64
  readPageEnt base shft = do 
    let base'    = fromIntegral (ptrToIntPtr base)
        ent_addr = base' + 8 * shft
    peek (intPtrToPtr (fromIntegral ent_addr))
-}
#else
translateForeignAddress :: DomId -> VCPU -> Ptr a -> Xen MFN
translateForeignAddress did vcpu remAddr = do
  ctxt <- domainRegisterContext did vcpu
  lookupPD $ fromIntegral ((cr3 . ctrlreg) ctxt)
 where
  ra :: Word32
  ra = fromIntegral $ ptrToIntPtr remAddr
  --
  makeMFN :: Word32 -> MFN
  makeMFN = MFN . fromIntegral
  --
  lookupPD :: Word32 -> Xen MFN
  lookupPD pdpe = do
    pd <- mapForeignMachineFramesReadOnly did [makeMFN $ pdpe `shiftR` 12]
    pde <- readPageEnt pd 22
    unmapForeignMachineFrames pd 4096
    lookupPT pde
  --
  lookupPT :: Word32 -> Xen MFN
  lookupPT pde
    | pde .&. 1 == 0 = xThrow EINVAL
    | otherwise = do
        pt <- mapForeignMachineFramesReadOnly did [makeMFN $ pde `shiftR` 12]
        pte <- readPageEnt pt 12
        unmapForeignMachineFrames pt 4096
        if (pte .&. 1) == 0
           then xThrow EINVAL
           else return $ makeMFN (pte `shiftR` 12)
  --
  readPageEnt :: VPtr a -> Int -> IO Word32
  readPageEnt base shft = do 
    let base'    = fromIntegral (ptrToIntPtr base)
        ent_addr = base' + 4 * ((ra `shiftR` shft) .&. 0x3ff)
    peek (intPtrToPtr (fromIntegral ent_addr))
#endif

-- |Map a set of foreign machine frame numbers into the address space read-only,
-- returning the new address.
mapForeignMachineFramesReadOnly :: DomId -> [MFN] -> Xen (VPtr a)
mapForeignMachineFramesReadOnly (DomId dom) mfns = do
  withArray mfns $ \ ptr -> do
    res <- map_readonly_frames dom ptr $ fromIntegral $ length mfns
    if res == nullPtr
      then xThrow ENOMEM
      else return $ castPtr res

-- |Map a machine frame number into the address space, returning
-- the new page.
mapForeignMachineFrames :: DomId -> [MFN] -> Xen (VPtr a)
mapForeignMachineFrames (DomId dom) mfns =
  withArray mfns (\ ptr -> do
    res <- map_frames dom ptr $ fromIntegral $ length mfns
    if res == nullPtr
       then xThrow ENOMEM
       else return $ castPtr res)

-- |Unmap pages mapped from another domain's page frame set.
unmapForeignMachineFrames :: VPtr a -> Int -> IO ()
unmapForeignMachineFrames ptr size = do
  unback_pages ptr (ptr `plusPtr` size) 0
  disclaim_vspace ptr (ptr `plusPtr` size)

-- |Convert a virtual address into a machine-physical address.
virtualToMachine :: VPtr a -> Xen (MPtr a)
virtualToMachine x = do
  res <- virtual_to_machine x
  if res == 0
    then xThrow EINVAL
    else return $ MPtr res

-- |Convert a machine-physical address into a virtual address.
machineToVirtual :: MPtr a -> Xen (VPtr a)
machineToVirtual (MPtr x) = machine_to_virtual x >>= \ x'->
  if x' == nullPtr
    then xThrow EINVAL
    else return x'

-- |Reserve a contiguous set of virtual frames, but do not use 
-- physical memory to back them. This means that an access to
-- the returned addresses will cause a page fault.
reserveVirtualFrames :: Word32 -> Xen (VPtr a)
reserveVirtualFrames num = do 
  va <- claim_vspace nullPtr (fromIntegral num * 4096)
  if va == nullPtr then xThrow ENOMEM else return $ va

-- |Unreserve a previously reserved set of virtual frames, so
-- that the rest of the system can use them again.
unreserveVirtualFrames :: Word32 -> VPtr a -> IO ()
unreserveVirtualFrames num va =
  disclaim_vspace va (va `plusPtr` (fromIntegral $ num * 4096))

-- |Determine if the given address is actually backed with some
-- physical page, thus determining whether or not someone can 
-- read or write from the address.
addressMapped :: VPtr a -> IO Bool
addressMapped addr = (/= 0) `fmap` address_mapped addr

--
-- --------------------------------------------------------------------------
--

#define C_PFN_T Word32

#if defined(CONFIG_X86_64) || defined(CONFIG_X86_PAE)
# define C_MADDR_T Word64
#else
# define C_MADDR_T Word32
#endif
#define C_PADDR_T Word32
#define C_VADDR_T (VPtr a)

-- Functions from mm.h
foreign import ccall unsafe "mm.h claim_vspace" 
  claim_vspace :: C_VADDR_T -> CSize -> IO C_VADDR_T
foreign import ccall unsafe "mm.h disclaim_vspace" 
  disclaim_vspace :: C_VADDR_T -> C_VADDR_T -> IO ()
foreign import ccall unsafe "mm.h alloc_page" 
  alloc_page :: IO C_VADDR_T
foreign import ccall unsafe "mm.h free_page" 
  free_page :: C_VADDR_T -> IO ()
foreign import ccall unsafe "mm.h virtual_to_machine" 
  virtual_to_machine :: C_VADDR_T -> IO C_MADDR_T
foreign import ccall unsafe "mm.h machine_to_virtual" 
  machine_to_virtual :: C_MADDR_T -> IO C_VADDR_T
foreign import ccall unsafe "mm.h address_mapped" 
  address_mapped :: C_VADDR_T -> IO Int
foreign import ccall unsafe "mm.h mark_as_page_table" 
  mark_as_page_table :: Int -> C_VADDR_T -> Word16 -> IO Int
foreign import ccall unsafe "mm.h mark_as_page_table_mfn" 
  mark_as_page_table_mfn :: Int -> MFN -> Word16 -> IO Int
foreign import ccall unsafe "mm.h set_page_writable" 
  set_page_writable :: C_VADDR_T -> Bool -> Word16 -> IO Int
foreign import ccall unsafe "mm.h map_frames" 
  map_frames :: Word16 -> Ptr MFN -> Word32 -> IO (VPtr a)
foreign import ccall unsafe "mm.h map_readonly_frames"
  map_readonly_frames :: Word16 -> Ptr MFN -> Word32 -> IO (VPtr a)
foreign import ccall unsafe "mm.h unback_pages" 
  unback_pages :: C_VADDR_T -> C_VADDR_T -> Int -> IO ()
foreign import ccall unsafe "mm.h system_wmb" 
  systemWMB :: IO ()
foreign import ccall unsafe "mm.h system_rmb" 
  systemRMB :: IO ()
foreign import ccall unsafe "mm.h system_mb" 
  systemMB :: IO ()


-- Functions from sys/mman.h
foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: VPtr () -> Int -> Int -> Int -> Int -> Int -> IO (VPtr a)


-- Functions from gnttab.h
foreign import ccall unsafe "gnttab.h gnttab_grant_access" 
  gnttab_grant_access :: Word16 -> Word16 -> VPtr a -> Bool -> IO ()
foreign import ccall unsafe "gnttab.h gnttab_end_access" 
  gnttab_end_access :: Word16 -> IO Bool
foreign import ccall unsafe "gnttab.h gnttab_grant_foreign_transfer_ref" 
  gnttab_grant_foreign_transfer_ref :: Word16 -> Word16 -> IO Int32
foreign import ccall unsafe "gnttab.h gnttab_reset_foreign_transfer_ref" 
  gnttab_reset_foreign_transfer_ref :: Word16 -> IO Int32
foreign import ccall unsafe "gnttab.h gnttab_finish_foreign_transfer_ref" 
  gnttab_finish_foreign_transfer_ref :: Word16 -> IO (VPtr a)
foreign import ccall unsafe "gnttab.h gnttab_transfer_page_to_dom" 
  transfer_page_to_dom :: VPtr a -> Word16 -> Word16 -> IO Int32
foreign import ccall unsafe "gnttab.h gnttab_address_of" 
  gnttab_address_of :: Word16 -> IO (VPtr a)
foreign import ccall unsafe "gnttab.h gnttab_map_grant_ref" 
  gnttab_map_grant_ref :: VPtr a -> Word16 -> Word32 -> Bool -> IO Int32
foreign import ccall unsafe "gnttab.h gnttab_unmap_grant_ref" 
  gnttab_unmap_grant_ref :: VPtr a -> Word32 -> IO Int32
