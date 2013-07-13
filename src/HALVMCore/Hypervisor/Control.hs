-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Various ways to control domains in execution. Most of these routines
-- will require the domain to be privileged in order to execute on anything
-- other than itself.
module Hypervisor.Control(
         CreateFlag(..)
       , createDomain
       , destroyDomain
       , pauseDomain
       , unpauseDomain
       , hypercallInit
       , setDomainHandle
       , getDomainCPUAffinity
       , setDomainCPUAffinity
       , setDomainMaxVCPUs
       )
 where

import Data.Data
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.Hypercalls
import Hypervisor.Memory
import Hypervisor.Structures.CPUMap

data CreateFlag = CreateHVM | CreateWithHAP | CreateWithS3Integ
                 | CreateNoOOSPageTables
 deriving (Eq, Show, Typeable, Data)

-- |Create a domain, using the given SSID and Handle. If provided, the domain
-- ID will be used as the default ID for the domain. However, its use is not
-- (as far as I can tell) guaranteed, so you should check your output.
createDomain :: Maybe DomId -> SID -> DomainHandle -> [CreateFlag] -> IO DomId
createDomain mdom sid hndl flags =
  domainControlOp DomCtlCreateDomain domid
    (buildCreateDomainCall sid hndl flags)
    (\ d _ _ -> return (toDomId d))
 where
  domid = case mdom of
            Just x  -> fromDomId x
            Nothing -> 0

destroyDomain :: DomId -> IO ()
destroyDomain dom =
  domainControlOp DomCtlDestroyDomain (fromDomId dom)
    (\ _ -> return ())
    (\ _ _ _ -> return ())

pauseDomain :: DomId -> IO ()
pauseDomain dom =
  domainControlOp DomCtlPauseDomain (fromDomId dom)
    (\ _ -> return ())
    (\ _ _ _ -> return ())

unpauseDomain :: DomId -> IO ()
unpauseDomain dom =
  domainControlOp DomCtlUnpauseDomain (fromDomId dom)
    (\ _ -> return ())
    (\ _ _ _ -> return ())

hypercallInit :: DomId -> MFN -> IO ()
hypercallInit dom mfn =
  domainControlOp DomCtlHypercallInit (fromDomId dom)
    (\ p -> poke p (fromIntegral (fromMFN mfn) :: Word64))
    (\ _ _ _ -> return ())

setDomainHandle :: DomId -> DomainHandle -> IO ()
setDomainHandle dom hndl =
  domainControlOp DomCtlSetDomainHandle (fromDomId dom)
    (\ p -> poke p hndl)
    (\ _ _ _ -> return ())

getDomainCPUAffinity :: DomId -> VCPU -> IO CPUMap
getDomainCPUAffinity dom vcpu = do
  domainControlOp DomCtlGetDomainCPUAffinity (fromDomId dom)
    (\ p -> poke p (fromVCPU vcpu :: Word32))
    (\ _ _ p -> readSerializedCPUMap (castPtr p))

setDomainCPUAffinity :: DomId -> VCPU -> CPUMap -> IO ()
setDomainCPUAffinity dom vcpu cmap =
  withCPUMapWriter cmap $ \ writer -> do
    domainControlOp DomCtlSetDomainCPUAffinity (fromDomId dom)
      (buildCPUAffinityReq vcpu writer)
      (\ _ _ _ -> return ())

setDomainMaxVCPUs :: DomId -> Word32 -> IO ()
setDomainMaxVCPUs dom numcpus =
  domainControlOp DomCtlSetDomainMaxVCPUs (fromDomId dom)
    (\ p -> poke p numcpus)
    (\ _ _ _ -> return ())


