-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Hypercalls.DomainControl(
         DomainControlOp(..)
       , domainControlOp
       , buildCreateDomainCall
       , readCPUAffinity
       , buildCPUAffinityReq
       , buildIRQPermissionReq
       , buildIOMemPermReq
       , buildIOPortPermReq
       , buildVCPUContextRequest
       )
 where

import Control.Exception
import Data.Bits
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import {-# SOURCE #-} Hypervisor.Control(CreateFlag(..))
import {-# SOURCE #-} Hypervisor.DomainInfo(SID, DomainHandle, VCPU, fromVCPU)
import Hypervisor.ErrorCodes
import Hypervisor.Structures.CPUMap
import Hypervisor.Structures.VCPUContext

#include <stdint.h>
#define __XEN_TOOLS__
#include <xen/domctl.h>
#include "ghcplatform.h"

data DomainControlOp = DomCtlCreateDomain
                     | DomCtlDestroyDomain
                     | DomCtlGetContext
                     | DomCtlGetDomainInfo
                     | DomCtlGetDomainCPUAffinity
                     | DomCtlHypercallInit
                     | DomCtlMaxMem
                     | DomCtlPauseDomain
                     | DomCtlSetContext
                     | DomCtlSetDomainCPUAffinity
                     | DomCtlSetDomainHandle
                     | DomCtlSetDomainMaxVCPUs
                     | DomCtlSetIOMemoryPerm
                     | DomCtlSetIOPortPerm
                     | DomCtlSetIRQPerm
                     | DomCtlUnpauseDomain

dcCmdVal :: DomainControlOp -> Word32
dcCmdVal DomCtlCreateDomain         = (#const XEN_DOMCTL_createdomain)
dcCmdVal DomCtlDestroyDomain        = (#const XEN_DOMCTL_destroydomain)
dcCmdVal DomCtlGetContext           = (#const XEN_DOMCTL_getvcpucontext)
dcCmdVal DomCtlGetDomainInfo        = (#const XEN_DOMCTL_getdomaininfo)
dcCmdVal DomCtlGetDomainCPUAffinity = (#const XEN_DOMCTL_getvcpuaffinity)
dcCmdVal DomCtlHypercallInit        = (#const XEN_DOMCTL_hypercall_init)
dcCmdVal DomCtlMaxMem               = (#const XEN_DOMCTL_max_mem)
dcCmdVal DomCtlPauseDomain          = (#const XEN_DOMCTL_pausedomain)
dcCmdVal DomCtlSetContext           = (#const XEN_DOMCTL_setvcpucontext)
dcCmdVal DomCtlSetDomainCPUAffinity = (#const XEN_DOMCTL_setvcpuaffinity)
dcCmdVal DomCtlSetDomainHandle      = (#const XEN_DOMCTL_setdomainhandle)
dcCmdVal DomCtlSetDomainMaxVCPUs    = (#const XEN_DOMCTL_max_vcpus)
dcCmdVal DomCtlSetIOMemoryPerm      = (#const XEN_DOMCTL_iomem_permission)
dcCmdVal DomCtlSetIOPortPerm        = (#const XEN_DOMCTL_ioport_permission)
dcCmdVal DomCtlSetIRQPerm           = (#const XEN_DOMCTL_irq_permission)
dcCmdVal DomCtlUnpauseDomain        = (#const XEN_DOMCTL_unpausedomain)

domainControlOp :: DomainControlOp -> Word16 ->
                   (Ptr a -> IO b)           ->
                   (Word16 -> b -> Ptr a -> IO c)      ->
                   IO c
domainControlOp op target setter getter =
  bracket (mallocBytes (#size xen_domctl_t)) free $ \ buffer -> do
    bzero buffer (#size xen_domctl_t)
    (#poke xen_domctl_t, cmd)               buffer (dcCmdVal op)
    (#poke xen_domctl_t, interface_version) buffer
          ((#const XEN_DOMCTL_INTERFACE_VERSION) :: Word32)
    (#poke xen_domctl_t, domain)            buffer target
    let argp = buffer `plusPtr` (#offset xen_domctl_t,u)
    setterres <- setter argp
    initres   <- do_domctl_op buffer
    if initres == 0
      then do rdom <- (#peek xen_domctl_t, domain) buffer
              getter rdom setterres argp
      else throw (toEnum (-initres) :: ErrorCode)

buildCreateDomainCall :: SID -> DomainHandle -> [CreateFlag] ->
                         Ptr a ->
                         IO ()
buildCreateDomainCall sid hndl flags ptr = do
  (#poke xen_domctl_createdomain_t,ssidref) ptr sid
  (#poke xen_domctl_createdomain_t,handle)  ptr hndl
  (#poke xen_domctl_createdomain_t,flags)   ptr flags

readCPUAffinity :: Ptr a -> IO CPUMap
readCPUAffinity p =
  readSerializedCPUMap (p `plusPtr` (#offset xen_domctl_vcpuaffinity_t,cpumap))

buildCPUAffinityReq :: VCPU -> (Ptr a -> IO ()) -> Ptr a -> IO ()
buildCPUAffinityReq vcpu writer p = do
  (#poke xen_domctl_vcpuaffinity_t, vcpu) p (fromVCPU vcpu :: Word32)
  writer (p `plusPtr` (#offset xen_domctl_vcpuaffinity_t,cpumap))

buildIRQPermissionReq :: Word8 -> Bool -> Ptr a -> IO ()
buildIRQPermissionReq pirq allow ptr = do
  (#poke xen_domctl_irq_permission_t,pirq)         ptr pirq
  (#poke xen_domctl_irq_permission_t,allow_access) ptr allow'
 where allow' = if allow then 1 else 0 :: Word8

buildIOMemPermReq :: Word64 -> Word64 -> Bool -> Ptr a -> IO ()
buildIOMemPermReq first num allow ptr = do
  (#poke xen_domctl_iomem_permission_t,first_mfn)    ptr first
  (#poke xen_domctl_iomem_permission_t,nr_mfns)      ptr num
  (#poke xen_domctl_iomem_permission_t,allow_access) ptr allow'
 where allow' = if allow then 1 else 0 :: Word8

buildIOPortPermReq :: Word32 -> Word32 -> Bool -> Ptr a -> IO ()
buildIOPortPermReq first num allow ptr = do
  (#poke xen_domctl_ioport_permission_t,first_port)   ptr first
  (#poke xen_domctl_ioport_permission_t,nr_ports)     ptr num
  (#poke xen_domctl_ioport_permission_t,allow_access) ptr allow'
 where allow' = if allow then 1 else 0 :: Word8

instance Storable [CreateFlag] where
  sizeOf    _   = sizeOf (undefined :: Word32)
  alignment _   = alignment (undefined :: Word32)
  peek      p   = do
    x <- peek (castPtr p) :: IO Word32
    return $ mAddFlag x (#const XEN_DOMCTL_CDF_hvm_guest) CreateHVM
           $ mAddFlag x (#const XEN_DOMCTL_CDF_hap) CreateWithHAP
           $ mAddFlag x (#const XEN_DOMCTL_CDF_s3_integrity) CreateWithS3Integ
           $ mAddFlag x (#const XEN_DOMCTL_CDF_oos_off) CreateNoOOSPageTables []
   where mAddFlag x c v r | c .&. x /= 0 = v : r
                          | otherwise    = r
  poke      p v = do
    let v' = mAddFlag (#const XEN_DOMCTL_CDF_hvm_guest) CreateHVM
           $ mAddFlag (#const XEN_DOMCTL_CDF_hap) CreateWithHAP
           $ mAddFlag (#const XEN_DOMCTL_CDF_s3_integrity) CreateWithS3Integ
           $ mAddFlag (#const XEN_DOMCTL_CDF_oos_off) CreateNoOOSPageTables 0
    poke (castPtr p) (v' :: Word32)
   where mAddFlag c x r | x `elem` v = c .|. r
                        | otherwise  = r

buildVCPUContextRequest :: Word32 -> Maybe ProcessorContext -> Ptr a ->
                           IO (Ptr ProcessorContext)
buildVCPUContextRequest v mContext reqp = do
  contextp <- mallocBytes (#size vcpu_guest_context_t)
  case mContext of
    Nothing -> return ()
    Just x  -> poke contextp x
  (#poke xen_domctl_vcpucontext_t,vcpu) reqp v
  (#poke xen_domctl_vcpucontext_t,ctxt) reqp contextp
  return contextp

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word -> IO ()

foreign import ccall unsafe "core-hypercalls.h do_domctl_op"
  do_domctl_op :: Ptr a -> IO Int
