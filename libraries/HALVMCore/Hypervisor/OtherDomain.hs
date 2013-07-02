-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Various routines for doing things to other domains. Running these will
-- almost certainly require a non-default security policy.
module Hypervisor.OtherDomain(
         allocForeignMachineFrames
       , updateOthersVAMapping
       , allocPortForDomain
       , setDomainMaxMemory
       , TLBEffect(..)
       , TLBTarget(..)
       )
 where

import Control.Exception
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Hypercalls
import Hypervisor.Memory
import Hypervisor.Port
import Hypervisor.Structures.CPUMap

-- |These flags tell Xen how to handle the TLB when it does a virtual address
-- mapping update.
data TLBEffect = NoFlush
               | InvPage TLBTarget
               | FlushTLB TLBTarget
  deriving (Eq, Show)

data TLBTarget = AllTLBs
               | MultipleTLBs (VPtr CPUMap)-- ^Should be at least 8-byte aligned
               | LocalTLB
  deriving (Eq, Show)

-- |Allocate the machine frames for a new foreign domain. This uses memory in
-- Xen's global free memory pool rather than your own memory space. Note,
-- again, that the return value is a list of machine frame numbers, not a list
-- of machine addresses. The arguments are the domain to create the memory for
-- and the amount of memory (in KBytes) to give the new domain.
allocForeignMachineFrames :: DomId -> Word32 -> IO [MFN]
allocForeignMachineFrames dom maxMemKB = do
  let num_pages = maxMemKB `div` 4
      num_pagesI = fromIntegral num_pages
      pfn_list = [0 .. num_pages-1]
  ptr <- mallocArray num_pagesI
  pokeArray ptr (map (toMFN . fromIntegral) pfn_list)
  populatePhysmap dom (fromIntegral num_pages) ptr
  res <- peekArray num_pagesI ptr
  free ptr
  return res

-- |Update the virtual address mapping of another domain.
updateOthersVAMapping :: DomId -> VPtr a -> Word -> TLBEffect -> IO ()
updateOthersVAMapping dom addr val tlbe =
  updateVAMappingOtherDomain (fromIntegral (ptrToWordPtr addr)) val tlbe dom

-- |Allocate an unbound port on behalf of the given domain. The first argument
-- is the domain on whose behalf you're allocating the port, and the second
-- argument is the domain that should be able to bind to that port.
allocPortForDomain :: DomId -> DomId -> IO Port
allocPortForDomain forDom remoteDom = do
  res <- evtchn_alloc_unbound (fromDomId forDom) (fromDomId remoteDom)
  case () of
    () | res < 0    -> throw (toEnum (fromIntegral (-res)) :: ErrorCode)
       | res < 1024 -> return (toPort res)
       | otherwise  -> throw ENOBUFS

-- |Set the maximum amount of memory a domain can use. The amount should be
-- given in kilobytes.
setDomainMaxMemory :: DomId -> Word64 -> IO ()
setDomainMaxMemory dom maxkb =
  domainControlOp DomCtlMaxMem (fromDomId dom)
    (\ reqp -> poke reqp maxkb)
    (\ _ _ _ -> return ())

foreign import ccall unsafe "events.h evtchn_alloc_unbound"
  evtchn_alloc_unbound :: Word16 -> Word16 -> IO Word32
