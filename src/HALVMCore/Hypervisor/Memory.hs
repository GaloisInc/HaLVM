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
         , mfnToMPtr, mptrToMFN , mptrToPtr, toMFN, fromMFN, toPFN
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
         , markFrameAsPageTable
         , mapFrames
         -- * Routines for creating or destroying grant references
         -- and grant handles.
         , GrantRef(..)
         , grantAccess
         , endAccess
         , GrantHandle(..)
         , mapGrants
         , unmapGrant
         -- * Routines for transferring or copying pages to another domain.
         , prepareTransfer
         , transferFrame
         , completeTransfer
         , performFrameCopy
         -- * Low-level routines for dealing with frames, address translation,
         -- and similar grungy things.
         , virtualToMachine
         , machineToVirtual
         , addressMapped
         , systemWMB, systemRMB, systemMB
         )
    where

import Control.Exception
import Control.Monad
import Data.Binary
import Data.Bits
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Generics
import Numeric

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes

--
-- * Types and conversions for dealing with memory.
--

-- |Pseudo-physical frame numbers. These frame numbers have very little to
-- do with the machine address or the virtual address, but are used in some
-- Xen hypercalls.
newtype PFN = PFN Word

-- |Translate to a PFN
toPFN :: Integral a => a -> PFN
toPFN x = PFN (fromIntegral x)

-- |Machine frame numbers. These frame numbers identify a phyical 4096-byte
-- frame on the underlying hardware.
newtype MFN = MFN Word
 deriving (Eq, Ord, Num, Read, Generic, Storable, Bits)

instance Show MFN where
  show (MFN x) = "MFN 0x" ++ showHex x ""

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
mfnToVPtr :: MFN -> IO (VPtr a)
mfnToVPtr = machineToVirtual . mfnToMPtr

-- |Convert a virtual address to the machine frame underlying its frame. This
-- function will perform the rounding for you. If the page is mapped (if
-- addressMapped) returns True, then this page is guaranteed to succeed.
vptrToMFN :: VPtr a -> IO MFN
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
allocPage :: IO (VPtr a)
allocPage = allocPageProt defaultProt

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
allocPageProt :: AllocProt -> IO (VPtr a)
allocPageProt flags = do
  va <- allocMemory nullPtr 4096 (getProt flags) 1
  if va == nullPtr then throw ENOMEM else return $! va

-- |Free a page allocated with allocPage.
freePage :: VPtr a -> IO ()
freePage x
  | x /= (x `alignPtr` 4096) = throw EINVAL
  | otherwise                = freeMemory x 4096

-- | Allocate a page, call a function with it, and free it.  Return the result
--   in the Xen monad, capturing the possibility of a page allocation failure.
withPage :: (VPtr a -> IO b) -> IO b
withPage f = allocPage >>= \page -> do
    b <- f page
    freePage page
    return b

-- |Set a page writable (or not).
setPageWritable :: VPtr a -> Bool -> IO ()
setPageWritable x val = do
  ent <- get_pt_entry x
  set_pt_entry x (modify ent)
 where
  modify a | val       = a `setBit` 1
           | otherwise = a `clearBit` 1

-- |Mark the given page as one that will be used as a page table.
-- The given address is a virtual address. This is the analagous
-- version of the MMUEXT_PIN_L?_TABLE case of the MMUext hypercall;
-- the argument specifying what level.
--
-- Note that changing your own page tables is a good way to crash,
-- unless you're very familiar with the HaLVM.
--
-- QUICK GUIDE:
--   Use level '1' for page tables
--   Use level '2' for page directories
--   Use level '3' for PAE base tables / directory pointer tables
--   Use level '4' for PML4
markAsPageTable :: Int -> VPtr a -> DomId -> IO ()
markAsPageTable l addr dom = do
  ent <- get_pt_entry addr
  let mfn' = fromIntegral (ent `shiftR` 12)
  markFrameAsPageTable l (MFN mfn') dom

markFrameAsPageTable :: Int -> MFN -> DomId -> IO ()
markFrameAsPageTable l mfn dom = do
  unless (l `elem` [1..4]) $ throw EINVAL
  pin_frame l (fromMFN mfn) (fromDomId dom) >>= standardUnitRes

-- |Map the given list of frames into a contiguous chunk of memory.
mapFrames :: [MFN] -> IO (VPtr a)
mapFrames mfns = withArray (map fromMFN mfns) $ \p -> mapFrames' p (length mfns)

--
-- * Routines for creating or destroying grant references and grant handles.
--

newtype GrantRef = GrantRef { unGrantRef :: Word32 }
 deriving (Eq, Ord, Generic, Storable)

instance Show GrantRef where
  show (GrantRef x) = "grant:" ++ show x

instance Read GrantRef where
  readsPrec d str =
    case splitAt 6 str of
      ("grant:",x) -> map (\ (g,rest) -> (GrantRef g, rest)) (readsPrec d x)
      _            -> []

instance Binary GrantRef where
  put (GrantRef r) = put r
  get              = GrantRef `fmap` get

-- |Grant access to a given domain to a given region of memory (starting at
-- the pointer and extending for the given length). The boolean determines
-- if the given domain will be able to write to the memory (True) or not
-- (False).
grantAccess :: DomId -> Ptr a -> Word -> Bool -> IO [GrantRef]
grantAccess dom ptr len writable = ga ptr (fromIntegral len)
 where
  ga _ 0 = return []
  ga p l = do
    let pword   = ptrToWordPtr ptr
        offset  = pword .&. 4095
        clength = minimum [4096, (4096 - offset), l]
        ro      = if writable then 0 else 1
    rptr <- malloc
    res <- allocGrant (fromDomId dom) p (fromIntegral clength) ro rptr
    when (res < 0) $ free rptr >> throw (toEnum (-res) :: ErrorCode)
    i <- peek rptr
    ((GrantRef i):) `fmap` ga (p `plusPtr` fromIntegral clength) (l - clength)

-- |Stop any access grants associated with the given grant reference.
endAccess :: GrantRef -> IO ()
endAccess (GrantRef gr) = do
  res <- endGrant gr
  when (res < 0) $ throw (toEnum (-res) :: ErrorCode)

-- |The type of a grant handle, or (in other words), the handle to a
-- grant from another domain that we've mapped.
newtype GrantHandle = GrantHandle [Word32]
  deriving (Eq, Ord, Show, Read)

-- |Map another domain's grants into our own address space. The return
-- values, if successful, are a pointer to the newly-mapped page in
-- memory and the grant handle. The boolean argument determines whether
-- HALVM should map the page read-only (False) or read\/write (True).
mapGrants :: DomId -> [GrantRef] -> Bool -> IO (VPtr a, GrantHandle)
mapGrants dom refs writable =
  withArray (map unGrantRef refs) $ \ ptr -> do
    let readonly = if writable then 0 else 1
        count    = length refs
        dom'     = fromDomId dom
    resptr  <- malloc
    hndlptr <- mallocArray count
    res <- mapGrants' dom' readonly ptr count resptr hndlptr nullPtr
    when (res /= 0) $ do
      free resptr >> free hndlptr
      when (res < 0) $ throw (toEnum (-res) :: ErrorCode)
      when (res > 0) $ throw (toEnum res :: GrantErrorCode)
    retptr <- peek resptr
    hnds   <- GrantHandle `fmap` peekArray count hndlptr
    free resptr >> free hndlptr
    return (retptr, hnds)

-- |Unmap the grant of another domain's page. This will make the shared
-- memory inaccessible.
unmapGrant :: GrantHandle -> IO ()
unmapGrant (GrantHandle gh) =
  withArray gh $ \ ptr -> do
    res <- unmapGrants ptr (length gh)
    when (res < 0) $ throw (toEnum (-res) :: ErrorCode)
    when (res > 0) $ throw (toEnum res :: GrantErrorCode)

--
-- * Routines for transferring or copying pages to another domain.
--

-- |Allow the given foreign domain to transfer a page to the running domain.
-- The resulting grant reference should be passed to the other domain, for
-- them to use in their transfer request. Usual protocol: Side A does
-- prepareTransfer, Side B does transferFrame, Side A does completeTransfer.
prepareTransfer :: DomId -> IO GrantRef
prepareTransfer dom = do
  res <- prepTransfer (fromDomId dom)
  when (res < 0) $ throw (toEnum (-res) :: ErrorCode)
  return (GrantRef (fromIntegral res))

-- |Transfer the given frame to another domain, using the given grant
-- reference as the transfer mechanism.
transferFrame :: DomId -> GrantRef -> VPtr a -> IO ()
transferFrame dom (GrantRef ref) ptr = do
  res <- transferGrant (fromDomId dom) ref ptr
  when (res < 0) $ throw (toEnum (-res) :: ErrorCode)
  when (res > 0) $ throw (toEnum   res  :: GrantErrorCode)

-- |Complete a grant transfer, returning the provided frame.
--
-- The first provided boolean determines the blocking behavior when the other
-- domain has not yet begun the transfer. If True, then the function will
-- block, under the assumption that the other side will begin the transfer
-- soon. If False, the function will not block, raising EAGAIN if the other
-- side has not yet begun the transfer. In all cases, if the other side has
-- begun the transfer, this routine will block until the transfer completes.
--
-- The second boolean determines if this grant reference should be recycled
-- and prepared for another grant transfer from the same domain upon completion
-- (True), or if the reference should be freed (False).
completeTransfer :: GrantRef -> Bool -> Bool -> IO MFN
completeTransfer gr@(GrantRef ref) block reset = do
  res <- compTransfer ref reset
  let ecode = toEnum (-res) :: ErrorCode
  case res of
    x | (x < 0) && block && (ecode == EAGAIN) -> completeTransfer gr block reset
      | (x < 0)                               -> throw ecode
      | otherwise                             -> return (MFN (fromIntegral x))

-- |Perform a copy of one frame to another frame. If two frame numbers are
-- used, they must be legitimate frame numbers for the calling domain. For
-- use between domains, the function can use grant references, which must
-- be set as read/write for the appropriate domains. The first mfn/ref and
-- domain is the source, the second set is the destination. Note that it is
-- an error to specify an MFN with any other identifier than domidSelf.
performFrameCopy :: (Either GrantRef MFN) -> DomId -> Word16 ->
                    (Either GrantRef MFN) -> DomId -> Word16 ->
                    Word16 ->
                    IO ()
performFrameCopy src sd soff dest dd doff len = do
  let (snum,sisref) = argToVals src sd
      (dnum,disref) = argToVals dest dd
  ret <- perform_grant_copy snum sisref srcDom soff dnum disref destDom doff len
  standardUnitRes ret
 where
  srcDom  = fromDomId sd
  destDom = fromDomId dd
  argToVals :: (Either GrantRef MFN) -> DomId -> (Word, Bool)
  argToVals (Left (GrantRef ref)) _ = (fromIntegral ref, True)
  argToVals (Right (MFN _)) dom | dom /= domidSelf =
    error "Called with an MFN and non-self domain!"
  argToVals (Right (MFN mfn)) _ = (fromIntegral mfn, False)

--
-- * Low-level routines for dealing with frames, address translation,
-- and similar grungy things.
--

-- |Convert a virtual address into a machine-physical address.
virtualToMachine :: VPtr a -> IO (MPtr a)
virtualToMachine x = do
  ent <- get_pt_entry x
  when (ent == 0) $ throw EINVAL
  unless (ent `testBit` 0) $ throw EINVAL
  let inword = ptrToWordPtr x
      inoff  = fromIntegral (inword .&. 4095)
      base   = ent .&. (complement 4095)
  return (MPtr (fromIntegral (base + inoff)))

-- |Convert a machine-physical address into a virtual address. THIS IS VERY
-- SLOW.
machineToVirtual :: MPtr a -> IO (VPtr a)
machineToVirtual (MPtr x) = machine_to_virtual x >>= \ x' -> do
  when (x' == nullPtr) $ throw EINVAL
  return x'

-- |Determine if the given address is actually backed with some
-- physical page, thus determining whether or not someone can
-- read or write from the address.
addressMapped :: VPtr a -> IO Bool
addressMapped addr = do
  ent <- get_pt_entry addr
  return (ent `testBit` 0) -- lowest bit is the present bit

--
-- --------------------------------------------------------------------------
--
standardUnitRes :: Integral a => a -> IO ()
standardUnitRes 0 = return ()
standardUnitRes x = throw (toEnum (fromIntegral (-x)) :: ErrorCode)

#define C_PFN_T Word32

#if defined(CONFIG_X86_64) || defined(CONFIG_X86_PAE)
# define C_MADDR_T Word64
# define C_SIZE_T  Word64
#else
# define C_MADDR_T Word32
# define C_SIZE_T  Word32
#endif
#define C_PADDR_T Word32
#define C_VADDR_T (VPtr a)

-- Functions from vmm.h
foreign import ccall unsafe "vmm.h get_pt_entry"
  get_pt_entry :: Ptr a -> IO Word64
foreign import ccall unsafe "vmm.h set_pt_entry"
  set_pt_entry :: Ptr a -> Word64 -> IO ()
foreign import ccall unsafe "vmm.h machine_to_virtual"
  machine_to_virtual :: C_MADDR_T -> IO (VPtr a)

-- Functions from memory.h
foreign import ccall unsafe "memory.h pin_frame"
  pin_frame :: Int -> Word -> Word32 -> IO Int
foreign import ccall unsafe "memory.h map_frames"
  mapFrames' :: VPtr Word -> Int -> IO (VPtr a)
foreign import ccall unsafe "memory.h system_wmb"
  systemWMB :: IO ()
foreign import ccall unsafe "memory.h system_rmb"
  systemRMB :: IO ()
foreign import ccall unsafe "memory.h system_mb"
  systemMB :: IO ()

-- Functions from runtime_reqs.h
foreign import ccall unsafe "runtime_reqs.h runtime_alloc"
  allocMemory :: VPtr a -> Word -> Int -> Int -> IO (VPtr a)
foreign import ccall unsafe "runtime_reqs.h runtime_free"
  freeMemory :: VPtr a -> Word -> IO ()

-- functions from grants.h
foreign import ccall unsafe "grants.h alloc_grant"
  allocGrant :: Word16 -> VPtr a -> Word16 -> Int -> VPtr Word32 -> IO Int
foreign import ccall unsafe "grants.h end_grant"
  endGrant :: Word32 -> IO Int
foreign import ccall unsafe "grants.h map_grants"
  mapGrants' :: Word16 -> Int -> VPtr Word32 -> Int ->
                VPtr (VPtr a) -> VPtr Word32 -> VPtr Word64 ->
                IO Int
foreign import ccall unsafe "grants.h unmap_grants"
  unmapGrants :: VPtr Word32 -> Int -> IO Int

foreign import ccall unsafe "grants.h prepare_transfer"
  prepTransfer :: Word16 -> IO Int
foreign import ccall unsafe "grants.h transfer_frame"
  transferGrant :: Word16 -> Word32 -> VPtr a -> IO Int
foreign import ccall unsafe "grants.h complete_transfer"
  compTransfer :: Word32 -> Bool -> IO Int
foreign import ccall unsafe "grants.h copy_frame"
  perform_grant_copy :: Word -> Bool -> Word16 -> Word16 ->
                        Word -> Bool -> Word16 -> Word16 ->
                        Word16 -> IO Int


