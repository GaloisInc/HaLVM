-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Hypercalls(
         module Hypervisor.Hypercalls.DomainControl
       , module Hypervisor.Hypercalls.SystemControl
       , module Hypervisor.Hypercalls.PhysicalDevice
       , updateVAMappingOtherDomain
       , populatePhysmap
#ifdef TESTING
       , parseTLBEffect
       , renderTLBEffect
#endif
       )
 where

import Control.Exception
import Data.Bits
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import {-# SOURCE #-} Hypervisor.OtherDomain(TLBEffect(..),TLBTarget(..))
import Hypervisor.DomainInfo
import Hypervisor.ErrorCodes
import Hypervisor.Hypercalls.DomainControl
import Hypervisor.Hypercalls.SystemControl
import Hypervisor.Hypercalls.PhysicalDevice

#include <stdint.h>
#include <sys/types.h>
#include <xen/xen.h>
#include <xen/memory.h>

renderTLBEffect :: TLBEffect -> Word
renderTLBEffect NoFlush      = (#const UVMF_NONE)
renderTLBEffect (InvPage x)  = (#const UVMF_INVLPG)    .|. (renderTarget x)
renderTLBEffect (FlushTLB x) = (#const UVMF_TLB_FLUSH) .|. (renderTarget x)

renderTarget :: TLBTarget -> Word
renderTarget AllTLBs          = (#const UVMF_ALL)
renderTarget (MultipleTLBs p) = (#const UVMF_MULTI) .|. (ptrToWord p)
renderTarget LocalTLB         = (#const UVMF_LOCAL)

ptrToWord :: Ptr a -> Word
ptrToWord = fromIntegral . ptrToWordPtr

#ifdef TESTING
parseTLBEffect :: Word -> TLBEffect
parseTLBEffect x
  | ftype == (#const UVMF_NONE)      = NoFlush
  | ftype == (#const UVMF_INVLPG)    = InvPage target
  | ftype == (#const UVMF_TLB_FLUSH) = FlushTLB target
  | otherwise                        = error "Bad input TLB effect value"
 where
  ftype  = x .&. (#const UVMF_FLUSHTYPE_MASK)
  ttype  = x .&. (#const UVMF_ALL)
  ptrw   = x .&. (complement ((#const UVMF_FLUSHTYPE_MASK).|.(#const UVMF_ALL)))
  ptr    = wordPtrToPtr (fromIntegral ptrw)
  target | ttype == 0 && ptrw == 0 = LocalTLB
         | ttype == 0              = MultipleTLBs ptr
         | otherwise               = AllTLBs
#endif

updateVAMappingOtherDomain :: Word -> Word64 -> TLBEffect -> DomId -> IO ()
updateVAMappingOtherDomain va newval flags dom = do
  res <- update_va_mapping_otherdomain va newval flags' dom'
  if res == 0
    then return ()
    else throw (toEnum (fromIntegral (-res)) :: ErrorCode)
 where
  flags' = renderTLBEffect flags
  dom'   = fromDomId dom

populatePhysmap :: DomId -> Word -> Ptr a -> IO ()
populatePhysmap dom num ptr = do
  rsvp <- mallocBytes (#size xen_memory_reservation_t)
  (#poke xen_memory_reservation_t, extent_start) rsvp ptr
  (#poke xen_memory_reservation_t, nr_extents)   rsvp num
  (#poke xen_memory_reservation_t, domid)        rsvp (fromDomId dom :: Word16)
  res <- do_memory_op (#const XENMEM_populate_physmap) rsvp
  free rsvp
  if res == 0
    then return ()
    else throw (toEnum (fromIntegral (-res)) :: ErrorCode)

foreign import ccall unsafe
  "hypercalls.h HYPERCALL_update_va_mapping_otherdomain"
  update_va_mapping_otherdomain :: Word -> Word64 -> Word -> Word16 -> IO Int

foreign import ccall unsafe
  "hypercalls.h HYPERCALL_memory_op"
  do_memory_op :: Word -> Ptr a -> IO Int

