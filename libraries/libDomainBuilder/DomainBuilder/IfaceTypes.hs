-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
{- 
   DomainBuilder.IfaceTypes defines types used in the interface to SivcS. -}
module DomainBuilder.IfaceTypes where
import Data.Word as Word
import qualified Data.Map as Map
import qualified Hypervisor.Basics as B

type Event = Word.Word16

newtype HVmId = HVmId Word.Word32
  deriving (Eq, Ord)

newtype VmId = VmId Word.Word32
  deriving (Eq, Ord, Show)

data Access -- from VmmTypes.thy
 = Access_none
 | Access_readOnly
 | Access_readWrite
 deriving (Eq, Show)

type VmName = String

data HyperCallError -- from VmmTypes.thy, extended with Xen errors
 = Error_vmRef
 | Error_forbidden
 | Error_remapSelfId
 | Error_selfHalt
 | Error_noEvent -- FIX - extra error for signal due to weaker typing
 | Error_badInternalResponse -- should never happen, but I would not bet on it
 | Error_unsupported -- FIX - should implement, but might be difficult on Xen
 | Error_unboundBootName
 | Error_internalHVmIdExhaustion
 | Error_internalDomIdExhaustion
 | Error_internalAllocPageFailed
 | Error_internalReserveVirtualFrames
 | Error_internalSetElemFailed
 | Error_internalOutOfMachineFrames
 | Error_internalPageTableAllocationAssertion
 | Error_xenAllocPort B.Err
 | Error_xenBindPort
 | Error_xenSetDomainMaxMemory B.Err
 | Error_xenDestroyDomain B.Err
 | Error_xenUpdateMapping B.Err
 | Error_xenGetContext B.Err
 | Error_xenSetContext B.Err
 | Error_xenCreateDomain B.Err
 | Error_xenDomainInfo B.Err
 | Error_xenSetDomainMaxVCpus B.Err
 | Error_xenMarkAsPageTable B.Err
 | Error_xenSetPageWritable B.Err
 | Error_xenAllocForeignMachineFrames B.Err
 | Error_xenMapForeignMachineFrames B.Err
 | Error_xenUnpause B.Err
 deriving (Eq, Show)

selfVmId :: VmId
selfVmId = VmId 0

-- REVISIT - consider newtypes
type VPage = Word32
type HPage = Word32

type RawGrantRef = Word16

type PageTrans = Map.Map VPage (VPage, Access)
 -- child virtual address -> parent virtual address

type PageMap = Map.Map VPage (HPage, Access)

type UReg = Word32

data URegs
 = URegs {uregs_eax :: UReg
         ,uregs_ecx :: UReg
         ,uregs_edx :: UReg 
         ,uregs_ebx :: UReg
         ,uregs_ebp :: UReg
         ,uregs_esi :: UReg
         ,uregs_edi :: UReg
         ,uregs_eip :: UReg
         ,uregs_esp :: UReg}
 deriving (Show)

data HyperCall
 = HyperCall_create B.DomId VmId [Event] [(VmId, VmId)]
 | HyperCall_destroy VmId
 | HyperCall_map VmId VPage VPage Access
 | HyperCall_unmap VPage
 | HyperCall_affiliate VmId VmId VmId
 | HyperCall_disaffiliate VmId
 | HyperCall_signal VmId Event
 -- "policy related hypercalls"
 -- - | hyp_advisePolicy 'policyAdvice
 -- - | hyp_adviseSchedule 'scheduleAdvice
 deriving Show

type HyperResult = Maybe HyperCallError

numEventChannels :: Word16
numEventChannels = 20
