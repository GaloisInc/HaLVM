-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Structures.DomainInfo(
         DomainInfoFlag(..)
       , DomainInfo(..)
       )
 where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import {-# SOURCE #-} Hypervisor.DomainInfo(DomId,SID,DomainHandle)
import {-# SOURCE #-} Hypervisor.DomainInfo(toDomId,fromDomId)
import {-# SOURCE #-} Hypervisor.Memory(fromMFN,toMFN,MFN)

#include <stdint.h>
#define __XEN_TOOLS__
#include <xen/domctl.h>
#include "ghcplatform.h"

-- |Domain flags
data DomainInfoFlag = DomainDying    | DomainHVM     | DomainShutdown
                    | DomainPaused   | DomainBlocked | DomainRunning
                    | DomainDebugged
  deriving (Eq, Ord, Generic, Show)

-- |Domain information
data DomainInfo = DomainInfo {
    diDomain          :: DomId
  , diFlags           :: [DomainInfoFlag]
  , diTotalPages      :: Word64
  , diMaxPages        :: Word64
  , diShrPages        :: Word64
  , diPagedPages      :: Word64
  , diSharedInfoFrame :: MFN
  , diCPUTime         :: Word64
  , diNumOnlineVCPUs  :: Word32
  , diMaxVCPUId       :: Word32
  , diSSIDRef         :: SID
  , diDomainHandle    :: DomainHandle
  , diCPUPool         :: Word32
  }
 deriving (Eq, Show)

instance Storable [DomainInfoFlag] where
  sizeOf    _   = sizeOf (undefined :: Word32)
  alignment _   = alignment (undefined :: Word32)
  peek      p   = do
    x <- peek (castPtr p) :: IO Word32
    return $ mAddFlag x DomainDying    (#const XEN_DOMINF_dying)
           $ mAddFlag x DomainHVM      (#const XEN_DOMINF_hvm_guest)
           $ mAddFlag x DomainShutdown (#const XEN_DOMINF_shutdown)
           $ mAddFlag x DomainPaused   (#const XEN_DOMINF_paused)
           $ mAddFlag x DomainBlocked  (#const XEN_DOMINF_blocked)
           $ mAddFlag x DomainRunning  (#const XEN_DOMINF_running)
           $ mAddFlag x DomainDebugged (#const XEN_DOMINF_debugged) []
   where mAddFlag x f c r | c .&. x /= 0 = f : r
                          | otherwise    = r
  poke      p v = do
    let x = mAddFlag DomainDying    (#const XEN_DOMINF_dying)
          $ mAddFlag DomainHVM      (#const XEN_DOMINF_hvm_guest)
          $ mAddFlag DomainShutdown (#const XEN_DOMINF_shutdown)
          $ mAddFlag DomainPaused   (#const XEN_DOMINF_paused)
          $ mAddFlag DomainBlocked  (#const XEN_DOMINF_blocked)
          $ mAddFlag DomainRunning  (#const XEN_DOMINF_running)
          $ mAddFlag DomainDebugged (#const XEN_DOMINF_debugged) 0
    poke (castPtr p) (x :: Word32)
   where mAddFlag f c rest | f `elem` v = c .|. rest
                           | otherwise  = rest

instance Storable DomainInfo where
  sizeOf _    = (#size xen_domctl_getdomaininfo_t)
  alignment _ = 8
  peek p      = do
    dm <- (#peek xen_domctl_getdomaininfo_t,domain)            p :: IO Word16
    fl <- (#peek xen_domctl_getdomaininfo_t,flags)             p
    tp <- (#peek xen_domctl_getdomaininfo_t,tot_pages)         p
    mx <- (#peek xen_domctl_getdomaininfo_t,max_pages)         p
    sh <- (#peek xen_domctl_getdomaininfo_t,shr_pages)         p
    pa <- (#peek xen_domctl_getdomaininfo_t,paged_pages)       p
    sf <- (#peek xen_domctl_getdomaininfo_t,shared_info_frame) p :: IO Word32
    cp <- (#peek xen_domctl_getdomaininfo_t,cpu_time)          p :: IO Word64
    vc <- (#peek xen_domctl_getdomaininfo_t,nr_online_vcpus)   p :: IO Word32
    mv <- (#peek xen_domctl_getdomaininfo_t,max_vcpu_id)       p :: IO Word32
    ss <- (#peek xen_domctl_getdomaininfo_t,ssidref)           p
    po <- (#peek xen_domctl_getdomaininfo_t,cpupool)           p :: IO Word32
    let handle_offset = (#offset xen_domctl_getdomaininfo_t,handle)
    ha <- peek (castPtr (p `plusPtr` handle_offset))
    return (DomainInfo (toDomId dm) fl
                       tp mx sh pa (toMFN (fromIntegral sf)) cp vc mv
                       ss ha po)
  poke p x    = do
    let d = fromDomId (diDomain x) :: Word16
    (#poke xen_domctl_getdomaininfo_t,domain)            p d
    (#poke xen_domctl_getdomaininfo_t,flags)             p (diFlags x)
    (#poke xen_domctl_getdomaininfo_t,tot_pages)         p (diTotalPages x)
    (#poke xen_domctl_getdomaininfo_t,max_pages)         p (diMaxPages x)
    (#poke xen_domctl_getdomaininfo_t,shr_pages)         p (diShrPages x)
    (#poke xen_domctl_getdomaininfo_t,paged_pages)       p (diPagedPages x)
    let m         = fromIntegral (fromMFN (diSharedInfoFrame x))
    (#poke xen_domctl_getdomaininfo_t,shared_info_frame) p (m :: Word64)
    (#poke xen_domctl_getdomaininfo_t,cpu_time)          p (diCPUTime x)
    (#poke xen_domctl_getdomaininfo_t,nr_online_vcpus)   p (diNumOnlineVCPUs x)
    (#poke xen_domctl_getdomaininfo_t,max_vcpu_id)       p (diMaxVCPUId x)
    (#poke xen_domctl_getdomaininfo_t,ssidref)           p (diSSIDRef x)
    (#poke xen_domctl_getdomaininfo_t,cpupool)           p (diCPUPool x)
    let handle_offset = (#offset xen_domctl_getdomaininfo_t,handle)
    poke (castPtr (p `plusPtr` handle_offset)) (diDomainHandle x)


