-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |Access to privileged hypercalls. Use at your own risk, you can probably 
-- destroy your hypervisor calling these wrong, and similar warnings.
--
-- NOTE #1: You must patch and rebuild Xen before using many of these. Unless 
-- you absolutely need to use these calls, you probably don't want to do that,
-- because it effectively grants privileged access to any domain. Thus 
-- destroying Xen's internal security.
--
-- NOTE #2: All the documentation in this file should be considered an 
-- enlightened guess. I (ACW) think I understand what these calls do, but
-- there's pretty much nothing in the way of reliable documentation for the 
-- Xen internals, and they change pretty often. Use at your own risk, etc.,
-- etc..
--

{-# LANGUAGE RecordWildCards #-}

module Hypervisor.Privileged(
         createDomain, destroyDomain
       , pauseDomain, unpauseDomain
       , domainInfo, evilBadMyDomId
       , domainRegisterContext, setDomainRegisterContext
       , setDomainCPUAffinity, hypercallInit, setDomainHandle
       , setDomainMaxVCPUs
       , readEmergencyConsole
       --
       , domainMemoryList, domainPageFrameInfo
       , setDomainMaxMemory, allocForeignMachineFrames
       , domainPageFrameInfo2
       , memtypeAdd, memtypeDelete, memtypeRead
       --
       , hostPhysicalInfo, currentSchedulerId
       , listDomainInfo, initIOPorts
       , setIRQPermission, setIOPortPermission, setIOMemoryPermission
       , setIOPrivilegeLevel, sendEOI
       --
       , updateOthersVAMapping
       , allocPortForDomain
       --
       , DomainInfo(..), HostPhysicalInfo(..)
       , RegisterContext(..)
       , ControlRegisterSet(..), DebugRegisterSet(..)
       , TrapInfo(..)
       , UserRegs(..)
       , PageFlag(..)
       , CPUMap, UUID
       )
 where

import Control.Exception(assert)
import Data.Bits
import Data.Maybe(fromMaybe)
import Data.Word
import Data.Word10
import Foreign.C.String
import Foreign.C.Types(CChar, CULong)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Basics
import Hypervisor.Port

#include <privileged.h>

-- |Describes a domain. Note that if you were expecting a name field of
-- some sort, sorry, but that's not available in the hypervisor. (It's 
-- internal to XenD).
data DomainInfo = DomainInfo { domain                   :: DomId 
                             , dying                    :: Bool
                             , hvm_guest                :: Bool
                             , shutdown                 :: Bool
                             , paused                   :: Bool
                             , blocked                  :: Bool
                             , running                  :: Bool
#if defined(XEN_DOMINF_cpumask) || #defined(DOMFLAGS_CPUSHIFT)
                             , cpu                      :: Word8
#endif
                             , shutdown_code            :: Word8
                             , tot_pages                :: Word64
                             , max_pages                :: Word64
                             , shr_pages                :: Word64
                             , shared_info_frame        :: Word64
                             , cpu_time                 :: Word64
                             , nr_online_vcpus          :: Word32
                             , max_vcpu_id              :: Word32
                             , ssidref                  :: SID
                             , domain_handle            :: UUID
                             }
  deriving (Show, Eq)

-- |Describes the physical platform this domain is running on.
data HostPhysicalInfo = HostPhysicalInfo {
    threads_per_core :: Word32
  , cores_per_socket :: Word32
  , num_cpus         :: Word32
  , max_node_id      :: Word32
  , cpu_khz          :: Word32
  , total_pages      :: Word64
  , free_pages       :: Word64
  , scrub_pages      :: Word64
  , hw_capability    :: [Word32]
  , max_cpu_id       :: Word32
#if XEN_SYSCTL_INTERFACE_VERSION < 0x00000008
  , cpu_to_node      :: Ptr Word32
#endif
  , capabilities     :: Word32
  }

-- |The processor state of a (presumably non-running) VM. The 'Context' is
-- used in the sense of 'a context switch'.
#if defined(CONFIG_X86_64)

data Arch = Arch
    { syscall_callback_eip   :: CULong
    , fs_base                :: Word64
    , gs_base_kernel         :: Word64
    , gs_base_user           :: Word64
    , user_regs              :: UserRegs
    }
  deriving (Show, Eq)

peekArchSpecific :: Ptr RegisterContext -> IO Arch
peekArchSpecific ptr = do
    user_regs              <- (#peek vcpu_guest_context_t, user_regs           ) ptr
    syscall_callback_eip   <- (#peek vcpu_guest_context_t, syscall_callback_eip) ptr
    fs_base                <- (#peek vcpu_guest_context_t, fs_base             ) ptr
    gs_base_kernel         <- (#peek vcpu_guest_context_t, gs_base_kernel      ) ptr
    gs_base_user           <- (#peek vcpu_guest_context_t, gs_base_user        ) ptr
    return Arch {..}

pokeArchSpecific :: Ptr RegisterContext -> Arch -> IO ()
pokeArchSpecific ptr arch = do
    (#poke vcpu_guest_context_t, user_regs           ) ptr (user_regs            arch)
    (#poke vcpu_guest_context_t, syscall_callback_eip) ptr (syscall_callback_eip arch)
    (#poke vcpu_guest_context_t, fs_base             ) ptr (fs_base              arch)
    (#poke vcpu_guest_context_t, gs_base_kernel      ) ptr (gs_base_kernel       arch)
    (#poke vcpu_guest_context_t, gs_base_user        ) ptr (gs_base_user         arch)

-- These fields are ordered by size and not struct order
data UserRegs = UserRegs { r15, r14, r13, r12, rbp, rbx, r11, r10, r9, r8, rax, rcx, rdx, rsi, rdi
                        , rip, rflags, rsp :: Word64
                        , error_code, entry_vector :: Word32
                        , cs, ss, es, ds, fs, gs :: Word16
                        , saved_upcall_mask :: Word8
                         }
  deriving (Show, Eq)

instance Storable UserRegs where
  sizeOf _ = (#size cpu_user_regs_t)
  alignment _ = 8
  peek ptr = do 
    r15 <- (#peek cpu_user_regs_t, r15) ptr
    r14 <- (#peek cpu_user_regs_t, r14) ptr
    r13 <- (#peek cpu_user_regs_t, r13) ptr
    r12 <- (#peek cpu_user_regs_t, r12) ptr
    rbp <- (#peek cpu_user_regs_t, rbp) ptr
    rbx <- (#peek cpu_user_regs_t, rbx) ptr
    r11 <- (#peek cpu_user_regs_t, r11) ptr
    r10 <- (#peek cpu_user_regs_t, r10) ptr
    r9  <- (#peek cpu_user_regs_t, r9) ptr
    r8  <- (#peek cpu_user_regs_t, r8) ptr
    rax <- (#peek cpu_user_regs_t, rax) ptr
    rcx <- (#peek cpu_user_regs_t, rcx) ptr
    rdx <- (#peek cpu_user_regs_t, rdx) ptr
    rsi <- (#peek cpu_user_regs_t, rsi) ptr
    rdi <- (#peek cpu_user_regs_t, rdi) ptr
    error_code <- (#peek cpu_user_regs_t, error_code) ptr
    entry_vector <- (#peek cpu_user_regs_t, entry_vector) ptr
    rip <- (#peek cpu_user_regs_t, rip) ptr
    cs <- (#peek cpu_user_regs_t, cs) ptr
    saved_upcall_mask <- (#peek cpu_user_regs_t, saved_upcall_mask) ptr
    rflags <- (#peek cpu_user_regs_t, rflags) ptr
    rsp <- (#peek cpu_user_regs_t, rsp) ptr
    ss <- (#peek cpu_user_regs_t, ss) ptr
    es <- (#peek cpu_user_regs_t, es) ptr
    ds <- (#peek cpu_user_regs_t, ds) ptr
    fs <- (#peek cpu_user_regs_t, fs) ptr
    gs <- (#peek cpu_user_regs_t, gs) ptr
    return $ UserRegs {..}
  poke ptr ur = do 
    (#poke cpu_user_regs_t, r15) ptr (r15 ur)
    (#poke cpu_user_regs_t, r14) ptr (r14 ur)
    (#poke cpu_user_regs_t, r13) ptr (r13 ur)
    (#poke cpu_user_regs_t, r12) ptr (r12 ur)
    (#poke cpu_user_regs_t, rbp) ptr (rbp ur)
    (#poke cpu_user_regs_t, rbx) ptr (rbx ur)
    (#poke cpu_user_regs_t, r11) ptr (r11 ur)
    (#poke cpu_user_regs_t, r10) ptr (r10 ur)
    (#poke cpu_user_regs_t, r9) ptr (r9 ur)
    (#poke cpu_user_regs_t, r8) ptr (r8 ur)
    (#poke cpu_user_regs_t, rax) ptr (rax ur)
    (#poke cpu_user_regs_t, rcx) ptr (rcx ur)
    (#poke cpu_user_regs_t, rdx) ptr (rdx ur)
    (#poke cpu_user_regs_t, rsi) ptr (rsi ur)
    (#poke cpu_user_regs_t, rdi) ptr (rdi ur)
    (#poke cpu_user_regs_t, error_code) ptr (error_code ur)
    (#poke cpu_user_regs_t, entry_vector) ptr (entry_vector ur)
    (#poke cpu_user_regs_t, rip) ptr (rip ur)
    (#poke cpu_user_regs_t, cs) ptr (cs ur)
    (#poke cpu_user_regs_t, saved_upcall_mask) ptr (saved_upcall_mask ur)
    (#poke cpu_user_regs_t, rflags) ptr (rflags ur)
    (#poke cpu_user_regs_t, rsp) ptr (rsp ur)
    (#poke cpu_user_regs_t, ss) ptr (ss ur)
    (#poke cpu_user_regs_t, es) ptr (es ur)
    (#poke cpu_user_regs_t, ds) ptr (ds ur)
    (#poke cpu_user_regs_t, fs) ptr (fs ur)
    (#poke cpu_user_regs_t, gs) ptr (gs ur)


#else
data Arch = Arch
    { event_callback_cs      :: CULong
    , failsafe_callback_cs   :: CULong
    , user_regs :: UserRegs
    }
  deriving (Show, Eq)

peekArchSpecific :: Ptr RegisterContext -> IO Arch
peekArchSpecific ptr = do
  event_callback_cs    <- (#peek vcpu_guest_context_t, event_callback_cs) ptr
  failsafe_callback_cs <- (#peek vcpu_guest_context_t, failsafe_callback_cs) ptr
  user_regs            <- (#peek vcpu_guest_context_t, user_regs) ptr
  return Arch {..}

pokeArchSpecific :: Ptr RegisterContext -> Arch -> IO ()
pokeArchSpecific ptr arch = do
  (#poke vcpu_guest_context_t, event_callback_cs) ptr (event_callback_cs arch)
  (#poke vcpu_guest_context_t, failsafe_callback_cs) ptr (failsafe_callback_cs arch)
  (#poke vcpu_guest_context_t, user_regs) ptr (user_regs arch)

-- These fields are ordered by size and not struct order
data UserRegs = UserRegs
                         { ebx, ecx, edx, esi, edi, ebp, eax, eip, eflags, esp :: Word32
                        , error_code, entry_vector, cs, ss, es, ds, fs, gs :: Word16
                        , saved_upcall_mask    :: Word8
                         }
  deriving (Show, Eq)

instance Storable UserRegs where
  sizeOf _ = (#size cpu_user_regs_t)
  alignment _ = 1
  peek ptr = do 
    bx <- (#peek cpu_user_regs_t, ebx) ptr
    cx <- (#peek cpu_user_regs_t, ecx) ptr
    dx <- (#peek cpu_user_regs_t, edx) ptr
    si <- (#peek cpu_user_regs_t, esi) ptr
    di <- (#peek cpu_user_regs_t, edi) ptr
    bp <- (#peek cpu_user_regs_t, ebp) ptr
    ax <- (#peek cpu_user_regs_t, eax) ptr
    ec <- (#peek cpu_user_regs_t, error_code) ptr
    ev <- (#peek cpu_user_regs_t, entry_vector) ptr
    ip <- (#peek cpu_user_regs_t, eip) ptr
    ma <- (#peek cpu_user_regs_t, saved_upcall_mask) ptr
    fl <- (#peek cpu_user_regs_t, eflags) ptr
    sp <- (#peek cpu_user_regs_t, esp) ptr
    sc <- (#peek cpu_user_regs_t, cs) ptr
    sg <- (#peek cpu_user_regs_t, ss) ptr
    se <- (#peek cpu_user_regs_t, es) ptr
    sd <- (#peek cpu_user_regs_t, ds) ptr
    sf <- (#peek cpu_user_regs_t, fs) ptr
    sh <- (#peek cpu_user_regs_t, gs) ptr
    return $ UserRegs bx cx dx si di bp ax ip fl sp ec ev sc sg se sd sf sh ma 
  poke ptr ur = do 
    (#poke cpu_user_regs_t, ebx) ptr (ebx ur)
    (#poke cpu_user_regs_t, ecx) ptr (ecx ur)
    (#poke cpu_user_regs_t, edx) ptr (edx ur)
    (#poke cpu_user_regs_t, esi) ptr (esi ur)
    (#poke cpu_user_regs_t, edi) ptr (edi ur)
    (#poke cpu_user_regs_t, ebp) ptr (ebp ur)
    (#poke cpu_user_regs_t, eax) ptr (eax ur)
    (#poke cpu_user_regs_t, error_code) ptr (error_code ur)
    (#poke cpu_user_regs_t, entry_vector) ptr (entry_vector ur)
    (#poke cpu_user_regs_t, eip) ptr (eip ur)
    (#poke cpu_user_regs_t, cs) ptr (cs ur)
    (#poke cpu_user_regs_t, saved_upcall_mask) ptr (saved_upcall_mask ur)
    (#poke cpu_user_regs_t, eflags) ptr (eflags ur)
    (#poke cpu_user_regs_t, esp) ptr (esp ur)
    (#poke cpu_user_regs_t, ss) ptr (ss ur)
    (#poke cpu_user_regs_t, es) ptr (es ur)
    (#poke cpu_user_regs_t, ds) ptr (ds ur)
    (#poke cpu_user_regs_t, fs) ptr (fs ur)
    (#poke cpu_user_regs_t, gs) ptr (gs ur)
#endif

data RegisterContext = RegisterContext 
    { fpu_ctxt               :: [Word8]
      -- ^The FPU context is exactly equivalent to the structure used for
      -- the FXSAVE instruction on Intel hardware. See also the FXSAVE 
      -- instruction in the Intel 64 and IA-32 Architectures Software
      -- Developer's Manual, Volume 2A.
    , cpu_flags              :: CULong
    , arch_specific          :: Arch
    , trap_ctxt              :: [TrapInfo]
    , ldt_base               :: CULong
    , ldt_ents               :: CULong
    , gdt_frames             :: [CULong]
    , gdt_ents               :: CULong
    , kernel_ss              :: CULong
    , kernel_sp              :: CULong
    , ctrlreg                :: ControlRegisterSet 
    , debugreg               :: DebugRegisterSet
    , event_callback_eip     :: CULong
    , failsafe_callback_eip  :: CULong
    , vm_assist              :: CULong
    }
  deriving (Show, Eq)


data ControlRegisterSet = ControlRegisterSet { cr0, cr1, cr2, cr3, cr4, cr5, cr6, cr7 :: CULong }
  deriving (Show, Eq)

data DebugRegisterSet = DebugRegisterSet { dr0, dr1, dr2, dr3, dr4, dr5, dr6, dr7 :: CULong }
  deriving (Show, Eq)

data TrapInfo = TrapInfo { vector       :: Word8
                         , trap_flags   :: Word8
                         , trap_cs      :: Word16
                         , address      :: CULong
                         }
  deriving (Show, Eq)



data PageFlag = PagePinned | Lvl1PageTable | Lvl2PageTable 
              | Lvl3PageTable | Lvl4PageTable | PageInvalid

type CPUMap = Word64
type UUID = [Word8]

--
-- --------------------------------------------------------------------------
--

#ifdef SPLIT_PRIVILEGED
# define CHOOSE(x, y) x
#else
# define CHOOSE(x, y) y
#endif

#define CREATE_OP    CHOOSE(XEN_DOMCTL_createdomain,DOM0_CREATEDOMAIN)
#define DESTROY_OP   CHOOSE(XEN_DOMCTL_destroydomain,DOM0_DESTROYDOMAIN)
#define PAUSE_OP     CHOOSE(XEN_DOMCTL_pausedomain,DOM0_PAUSEDOMAIN)
#define UNPAUSE_OP   CHOOSE(XEN_DOMCTL_unpausedomain,DOM0_UNPAUSEDOMAIN)
#define MAXVCPUS_OP  CHOOSE(XEN_DOMCTL_max_vcpus,DOM0_MAX_VCPUS)
#define DOMINFO_OP   CHOOSE(XEN_DOMCTL_getdomaininfo,DOM0_GETDOMAININFO)
#define GTREGCONT_OP CHOOSE(XEN_DOMCTL_getvcpucontext,DOM0_GETVCPUCONTEXT)
#define STREGCONT_OP CHOOSE(XEN_DOMCTL_setvcpucontext,DOM0_SETVCPUCONTEXT)
#define STAFFIN_OP   CHOOSE(XEN_DOMCTL_setvcpuaffinity,DOM0_SETVCPUAFFINITY)
#define HYPERINIT_OP CHOOSE(XEN_DOMCTL_hypercall_init,DOM0_HYPERCALL_INIT)
#define STDOMHNDL_OP CHOOSE(XEN_DOMCTL_setdomainhandle,DOM0_SETDOMAINHANDLE)
#define GTMLIST_OP   CHOOSE(XEN_DOMCTL_getmemlist,DOM0_GETMEMLIST)
#define GTFRMINFO_OP CHOOSE(XEN_DOMCTL_getpageframeinfo,DOM0_GETPAGEFRAMEINFO)
#define STMAXMEM_OP  CHOOSE(XEN_DOMCTL_max_mem,DOM0_SETDOMAINMAXMEM)
#define GETPFI2_OP   CHOOSE(XEN_DOMCTL_getpageframeinfo2,DOM0_GETPAGEFRAMEINFO2)
#define MTYPE_ADD_OP CHOOSE(XENPF_add_memtype,DOM0_ADD_MEMTYPE)
#define MTYPE_DEL_OP CHOOSE(XENPF_del_memtype,DOM0_DEL_MEMTYPE)
#define MTYPE_RD_OP  CHOOSE(XENPF_read_memtype,DOM0_READ_MEMTYPE)
#define PHYSINFO_OP  CHOOSE(XEN_SYSCTL_physinfo,DOM0_PHYSINFO)
#define SCHEDID_OP   CHOOSE(XEN_SYSCTL_sched_id,DOM0_SCHED_ID)
#define LDOMINFO_OP  CHOOSE(XEN_SYSCTL_getdomaininfolist,DOM0_GETDOMAININFOLIST)
#define STIRQPERM_OP CHOOSE(XEN_DOMCTL_irq_permission,DOM0_IRQ_PERMISSION)
#define STIOMPERM_OP CHOOSE(XEN_DOMCTL_iomem_permission,DOM0_IOMEM_PERMISSION)
#define STIOPPERM_OP CHOOSE(XEN_DOMCTL_ioport_permission,DOM0_IOPORT_PERMISSION)

class PrivilegedOpInfo a b | a -> b where
  pushRequestInfo  :: a -> Ptr a -> IO ()
  pullResponseInfo :: Ptr a -> IO b

data CreateData     = CreateData SID UUID DomId
data DestroyData    = DestroyData DomId
data PauseData      = PauseData DomId
data UnpauseData    = UnpauseData DomId
data MaxVCPUsData   = MaxVCPUsData DomId Word32
data ReadConData    = ReadConData Bool Bool Word32 Word32 (Ptr CChar)
data DomInfoData    = DomInfoData DomId
data GetRegContData = GetRegContData DomId Word32 (Ptr RegisterContext)
data SetRegContData = SetRegContData DomId Word32 (Ptr RegisterContext)
data SetAffinData   = SetAffinData DomId Word32 CPUMap
data HyperInitData  = HyperInitData DomId Word32
data SetDomHndlData = SetDomHndlData DomId UUID
data GetMemListData = GetMemListData DomId Word64 (Ptr Word32)
data GetFrameData   = GetFrameData DomId Word32
data SetMaxMemData  = SetMaxMemData DomId Word64
data GetPFI2Data    = GetPFI2Data DomId Word64 (Ptr Word32)
data MTypeAddData   = MTypeAddData Word32 Word64 Word32
data MTypeDelData   = MTypeDelData Word32 Word32
data MTypeRdData    = MTypeRdData Word32
data PhysInfoData   = PhysInfoData 
data SchedIdData    = SchedIdData
data LDomInfoData   = LDomInfoData DomId Word32 (Ptr DomainInfo)
data SetIRQPermData = SetIRQPermData DomId Word8 Word8
data SetIOPortData  = SetIOPortData DomId Word32 Word32 Word8
#ifdef SPLIT_PRIVILEGED
data SetIOMemData   = SetIOMemData DomId Word64 Word64 Word8
#else
data SetIOMemData   = SetIOMemData DomId Word32 Word64 Word8
#endif
instance PrivilegedOpInfo CreateData DomId where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (CreateData ssid hndl _) ptr = do
    (#poke xen_domctl_t, u.createdomain.ssidref) ptr ssid
    pokeArray ((#ptr xen_domctl_t, u.createdomain.handle) ptr) hndl
  pullResponseInfo ptr = DomId `fmap` (#peek xen_domctl_t, domain) ptr
#else
  pushRequestInfo (CreateData ssid handle (DomId dom)) ptr = do
    (#poke dom0_op_t, u.createdomain.domain) ptr dom
    (#poke dom0_op_t, u.createdomain.ssidref) ptr ssid
    pokeArray ((#ptr dom0_op_t, u.createdomain.handle) ptr) handle
  pullResponseInfo p = DomId `fmap` (#peek dom0_op_t, u.createdomain.domain) p
#endif

instance PrivilegedOpInfo DestroyData () where
  pushRequestInfo (DestroyData (DomId _dom)) _ptr =
#ifdef SPLIT_PRIVILEGED
    return ()
#else
    (#poke dom0_op_t, u.destroydomain.domain) _ptr _dom
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo PauseData () where
  pushRequestInfo (PauseData (DomId _dom)) _ptr = 
#ifdef SPLIT_PRIVILEGED
    return ()
#else
    (#poke dom0_op_t, u.pausedomain.domain) _ptr _dom
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo UnpauseData () where
  pushRequestInfo (UnpauseData (DomId _dom)) _ptr = do
#ifdef SPLIT_PRIVILEGED
    return ()
#else
    (#poke dom0_op_t, u.unpausedomain.domain) _ptr _dom
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo MaxVCPUsData () where
  pushRequestInfo (MaxVCPUsData (DomId _dom) num) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.max_vcpus.max) ptr num
#else
    (#poke dom0_op_t, u.max_vcpus.domain) ptr _dom
    (#poke dom0_op_t, u.max_vcpus.max) ptr num
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo ReadConData (Word32, Word32) where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (ReadConData clear incr index bsize buffer) ptr = do
    (#poke xen_sysctl_t, u.readconsole.clear) ptr (toWord8 clear)
    (#poke xen_sysctl_t, u.readconsole.incremental) ptr (toWord8 incr)
    (#poke xen_sysctl_t, u.readconsole.index) ptr index
    (#poke xen_sysctl_t, u.readconsole.buffer) ptr buffer
    (#poke xen_sysctl_t, u.readconsole.count) ptr bsize
   where 
    toWord8 :: Bool -> Word8
    toWord8 True  = 1
    toWord8 False = 0
  pullResponseInfo ptr = do
    index <- (#peek xen_sysctl_t, u.readconsole.index) ptr
    count <- (#peek xen_sysctl_t, u.readconsole.count) ptr
    return (index, count)
#else
# error "Write privilegedOpInfo stuff for non-split privileged"
#endif
  
instance PrivilegedOpInfo DomInfoData DomainInfo where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (DomInfoData _) _ = return ()
  pullResponseInfo ptr = (#peek xen_domctl_t, u.getdomaininfo) ptr
#else
  pushRequestInfo (DomInfoData (DomId dom)) ptr =
    (#poke dom0_op_t, u.getdomaininfo.domain) ptr dom
  pullResponseInfo ptr = (#peek dom0_op_t, u.getdomaininfo) ptr
#endif

instance PrivilegedOpInfo GetRegContData () where
  pushRequestInfo (GetRegContData (DomId _dom) vcpu ctxtptr) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.vcpucontext.vcpu)   ptr vcpu
    (#poke xen_domctl_t, u.vcpucontext.ctxt)   ptr ctxtptr
#else
    (#poke dom0_op_t, u.getvcpucontext.domain) ptr _dom
    (#poke dom0_op_t, u.getvcpucontext.vcpu)   ptr vcpu
    (#poke dom0_op_t, u.getvcpucontext.ctxt)   ptr ctxtptr
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo SetRegContData () where
  pushRequestInfo (SetRegContData (DomId _dom) vcpu ctxtptr) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.vcpucontext.vcpu)   ptr vcpu
    (#poke xen_domctl_t, u.vcpucontext.ctxt)   ptr ctxtptr
#else
    (#poke dom0_op_t, u.setvcpucontext.domain) ptr _dom
    (#poke dom0_op_t, u.setvcpucontext.vcpu)   ptr vcpu
    (#poke dom0_op_t, u.setvcpucontext.ctxt)   ptr ctxtptr
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo SetAffinData () where
  pushRequestInfo (SetAffinData (DomId _dom) vcpu cpumap) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.vcpuaffinity.vcpu)   ptr vcpu
    (#poke xen_domctl_t, u.vcpuaffinity.cpumap) ptr cpumap
#else
    (#poke dom0_op_t, u.setvcpuaffinity.domain) ptr _dom
    (#poke dom0_op_t, u.setvcpuaffinity.vcpu)   ptr vcpu
    (#poke dom0_op_t, u.setvcpuaffinity.cpumap) ptr cpumap
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo HyperInitData () where
  pushRequestInfo (HyperInitData (DomId _dom) mfn) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.hypercall_init.gmfn) ptr mfn
#else
    (#poke dom0_op_t, u.hypercall_init.domain)  ptr _dom
    (#poke dom0_op_t, u.hypercall_init.mfn)     ptr mfn
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo SetDomHndlData () where
  pushRequestInfo (SetDomHndlData (DomId _dom) h) ptr = do
#ifdef SPLIT_PRIVILEGED
    pokeArray ((#ptr xen_domctl_t,u.setdomainhandle.handle) ptr) h
#else
    (#poke dom0_op_t, u.setdomainhandle.domain) ptr _dom
    pokeArray ((#ptr dom0_op_t, u.setdomainhandle.handle) ptr) h
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo GetMemListData Word32 where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (GetMemListData (DomId _) max_pfns buffer) ptr = do
    (#poke xen_domctl_t, u.getmemlist.max_pfns) ptr max_pfns
    (#poke xen_domctl_t, u.getmemlist.buffer)   ptr buffer
  pullResponseInfo ptr = (#peek xen_domctl_t, u.getmemlist.num_pfns) ptr
#else
  pushRequestInfo (GetMemListData (DomId dom) max_pfns buffer) ptr = do
    (#poke dom0_op_t, u.getmemlist.domain)      ptr dom
    (#poke dom0_op_t, u.getmemlist.max_pfns)    ptr max_pfns
    (#poke dom0_op_t, u.getmemlist.buffer)      ptr buffer
  pullResponseInfo ptr = (#peek dom0_op_t, u.getmemlist.num_pfns) ptr
#endif

instance PrivilegedOpInfo GetFrameData [PageFlag] where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (GetFrameData (DomId _) mfn) ptr = do
    (#poke xen_domctl_t, u.getpageframeinfo.gmfn) ptr mfn
  pullResponseInfo ptr = do
    flags <- (#peek xen_domctl_t, u.getpageframeinfo.gmfn) ptr
    return $ parseFrameInfoFlags flags
#else
  pushRequestInfo (GetFrameData (DomId dom) mfn) ptr = do
    (#poke dom0_op_t, u.getpageframeinfo.domain) ptr dom
    (#poke dom0_op_t, u.getpageframeinfo.mfn)    ptr mfn
  pullResponseInfo ptr = do
    flags <- (#peek dom0_op_t, u.getpageframeinfo.type) ptr
    return $ parseFrameInfoFlags flags
#endif

instance PrivilegedOpInfo SetMaxMemData () where
  pushRequestInfo (SetMaxMemData (DomId _dom) maxMemKB) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.max_mem.max_memkb)      ptr maxMemKB
#else
    (#poke dom0_op_t, u.setdomainmaxmem.domain)    ptr _dom
    (#poke dom0_op_t, u.setdomainmaxmem.max_memkb) ptr maxMemKB
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo GetPFI2Data () where
  pushRequestInfo (GetPFI2Data (DomId _dom) num array) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.getpageframeinfo2.num)   ptr num
    (#poke xen_domctl_t, u.getpageframeinfo2.array) ptr array
#else
    (#poke dom0_op_t, u.getpageframeinfo2.domain)   ptr _dom
    (#poke dom0_op_t, u.getpageframeinfo2.num)      ptr num
    (#poke dom0_op_t, u.getpageframeinfo2.array)    ptr array
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo MTypeAddData (Word32, Word32) where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (MTypeAddData mfn nr_mfns mtype) ptr = do
    (#poke xen_platform_op_t, u.add_memtype.mfn)     ptr mfn
    (#poke xen_platform_op_t, u.add_memtype.nr_mfns) ptr nr_mfns
    (#poke xen_platform_op_t, u.add_memtype.type)    ptr mtype
  pullResponseInfo ptr = do
    hndl <- (#peek xen_platform_op_t, u.add_memtype.handle) ptr
    reg  <- (#peek xen_platform_op_t, u.add_memtype.reg)    ptr
    return (hndl, reg)
#else
  pushRequestInfo (MTypeAddData mfn nr_mfns mtype) ptr = do
    (#poke dom0_op_t, u.add_memtype.mfn)     ptr mfn
    (#poke dom0_op_t, u.add_memtype.nr_mfns) ptr nr_mfns
    (#poke dom0_op_t, u.add_memtype.type)    ptr mtype
  pullResponseInfo ptr = do
    res_hndl <- (#peek dom0_op_t, u.add_memtype.handle) ptr
    res_reg  <- (#peek dom0_op_t, u.add_memtype.reg)    ptr
    return (res_hndl, res_reg)
#endif

instance PrivilegedOpInfo MTypeDelData () where
  pushRequestInfo (MTypeDelData handle reg) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_platform_op_t, u.del_memtype.handle) ptr handle
    (#poke xen_platform_op_t, u.del_memtype.reg)    ptr reg
#else
    (#poke dom0_op_t, u.del_memtype.handle)         ptr handle
    (#poke dom0_op_t, u.del_memtype.reg)            ptr reg
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo MTypeRdData (Word32, Word64, Word32) where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (MTypeRdData reg) ptr = do
    (#poke xen_platform_op_t, u.read_memtype.reg) ptr reg
  pullResponseInfo ptr = do
    mfn     <- (#peek xen_platform_op_t, u.read_memtype.mfn)     ptr
    nr_mfns <- (#peek xen_platform_op_t, u.read_memtype.nr_mfns) ptr
    mtype   <- (#peek xen_platform_op_t, u.read_memtype.type)    ptr
    return (mfn, nr_mfns, mtype)
#else
  pushRequestInfo (MTypeRdData reg) ptr = do
    (#poke dom0_op_t, u.read_memtype.reg) ptr reg
  pullResponseInfo ptr = do
    mfn     <- (#peek dom0_op_t, u.read_memtype.mfn)     ptr
    nr_mfns <- (#peek dom0_op_t, u.read_memtype.nr_mfns) ptr
    mtype   <- (#peek dom0_op_t, u.read_memtype.type)    ptr
    return (mfn, nr_mfns, mtype)
#endif

instance PrivilegedOpInfo PhysInfoData HostPhysicalInfo where
  pushRequestInfo (PhysInfoData) _ = return ()
  pullResponseInfo ptr = peek (castPtr ptr)

instance PrivilegedOpInfo SchedIdData Word32 where
  pushRequestInfo (SchedIdData) _ = return ()
#ifdef SPLIT_PRIVILEGED
  pullResponseInfo ptr = (#peek xen_sysctl_t, u.sched_id.sched_id) ptr 
#else
  pullResponseInfo ptr = (#peek dom0_op_t, u.sched_id.sched_id) ptr
#endif

instance PrivilegedOpInfo LDomInfoData Word32 where
#ifdef SPLIT_PRIVILEGED
  pushRequestInfo (LDomInfoData (DomId dom) maxdoms buffer) ptr = do
    (#poke xen_sysctl_t, u.getdomaininfolist.first_domain) ptr dom
    (#poke xen_sysctl_t, u.getdomaininfolist.max_domains)  ptr maxdoms
    (#poke xen_sysctl_t, u.getdomaininfolist.buffer)       ptr buffer
  pullResponseInfo ptr =
    (#peek xen_sysctl_t, u.getdomaininfolist.num_domains) ptr
#else
  pushRequestInfo (LDomInfoData (DomId dom) maxdoms buffer) ptr = do
    (#poke dom0_op_t, u.getdomaininfolist.first_domain) ptr dom
    (#poke dom0_op_t, u.getdomaininfolist.max_domains)  ptr maxdoms
    (#poke dom0_op_t, u.getdomaininfolist.buffer)       ptr buffer
  pullResponseInfo ptr = 
    (#peek dom0_op_t, u.getdomaininfolist.num_domains) ptr
#endif

instance PrivilegedOpInfo SetIRQPermData () where
  pushRequestInfo (SetIRQPermData (DomId _dom) pirq allow) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.irq_permission.pirq)         ptr pirq
    (#poke xen_domctl_t, u.irq_permission.allow_access) ptr allow
#else
    (#poke dom0_op_t, u.irq_permission.domain)          ptr _dom
    (#poke dom0_op_t, u.irq_permission.pirq)            ptr pirq
    (#poke dom0_op_t, u.irq_permission.allow_access)    ptr allow
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo SetIOPortData () where
  pushRequestInfo (SetIOPortData (DomId _dom) first num allow) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.ioport_permission.first_port)   ptr first
    (#poke xen_domctl_t, u.ioport_permission.nr_ports)     ptr num
    (#poke xen_domctl_t, u.ioport_permission.allow_access) ptr allow
#else
    (#poke dom0_op_t, u.ioport_permission.domain)          ptr _dom
    (#poke dom0_op_t, u.ioport_permission.first_port)      ptr first
    (#poke dom0_op_t, u.ioport_permission.nr_ports)        ptr num
    (#poke dom0_op_t, u.ioport_permission.allow_access)    ptr allow
#endif
  pullResponseInfo _ = return ()

instance PrivilegedOpInfo SetIOMemData () where
  pushRequestInfo (SetIOMemData (DomId _dom) first num allow) ptr = do
#ifdef SPLIT_PRIVILEGED
    (#poke xen_domctl_t, u.iomem_permission.first_mfn)    ptr first
    (#poke xen_domctl_t, u.iomem_permission.nr_mfns)      ptr num
    (#poke xen_domctl_t, u.iomem_permission.allow_access) ptr allow
#else
    (#poke dom0_op_t, u.iomem_permission.domain)          ptr _dom
    (#poke dom0_op_t, u.iomem_permission.first_mfn)       ptr first
    (#poke dom0_op_t, u.iomem_permission.nr_mfns)         ptr num
    (#poke dom0_op_t, u.iomem_permission.allow_access)    ptr allow
#endif
  pullResponseInfo _ = return ()

--
-- --------------------------------------------------------------------------
--

#ifdef SPLIT_PRIVILEGED
dc_interface_version :: Word32
dc_interface_version = (#const XEN_DOMCTL_INTERFACE_VERSION)

sc_interface_version :: Word32
sc_interface_version = (#const XEN_SYSCTL_INTERFACE_VERSION)

p_interface_version  :: Word32
p_interface_version  = (#const XENPF_INTERFACE_VERSION)
#else
interface_version :: Word32
interface_version    = (#const DOM0_INTERFACE_VERSION)
#endif

runDomCtlOp :: PrivilegedOpInfo cmd res =>
               Int -> DomId -> cmd ->
               Xen res
runDomCtlOp cmd (DomId _dom) datum = do
#ifdef SPLIT_PRIVILEGED
  buffer <- mallocBytes (#size xen_domctl_t)
  bzero buffer (#size xen_domctl_t)
  (#poke xen_domctl_t, cmd)               buffer cmd
  (#poke xen_domctl_t, interface_version) buffer dc_interface_version
  (#poke xen_domctl_t, domain)            buffer _dom
  pushRequestInfo datum buffer
  init_res <- do_domctl_op buffer
  res <- if init_res == 0
            then pullResponseInfo buffer
            else xThrow $ toEnum (-init_res)
  free buffer
  return res
#else
  buffer <- mallocBytes (#size dom0_op_t)
  bzero buffer (#size dom0_op_t)
  (#poke dom0_op_t, cmd)               buffer cmd
  (#poke dom0_op_t, interface_version) buffer interface_version
  pushRequestInfo datum buffer
  init_res <- do_dom0_op buffer
  res <- if init_res == 0
            then pullResponseInfo buffer
            else xThrow $ toEnum (-init_res)
  free buffer
  return res
#endif

runSysCtlOp :: PrivilegedOpInfo cmd res =>
               Int -> cmd ->
               Xen res
runSysCtlOp cmd datum = do
#ifdef SPLIT_PRIVILEGED
  buffer <- mallocBytes (#size xen_sysctl_t)
  bzero buffer (#size xen_sysctl_t)
  (#poke xen_sysctl_t, cmd)               buffer cmd
  (#poke xen_sysctl_t, interface_version) buffer sc_interface_version
  pushRequestInfo datum buffer
  init_res <- do_sysctl_op buffer
  res <- if init_res == 0
            then pullResponseInfo buffer
            else xThrow $ toEnum (-init_res)
  free buffer
  return res
#else
  runDomCtlOp cmd undefined datum
#endif

runPlatOp :: PrivilegedOpInfo cmd res =>
             Int -> cmd ->
             Xen res
runPlatOp cmd datum = do
#ifdef SPLIT_PRIVILEGED
  buffer <- mallocBytes (#size xen_platform_op_t)
  bzero buffer (#size xen_platform_op_t)
  (#poke xen_platform_op_t, cmd)               buffer cmd
  (#poke xen_platform_op_t, interface_version) buffer p_interface_version
  pushRequestInfo datum buffer
  init_res <- do_platform_op buffer
  res <- if init_res == 0
            then pullResponseInfo buffer
            else xThrow $ toEnum (-init_res)
  free buffer
  return res
#else
  runDomCtlOp cmd undefined datum
#endif


--
-- --------------------------------------------------------------------------
--

-- |Create a domain, using the given SSIdRef and Handle. So far as I can tell,
-- these should be zero and a random number. This only creates an entry in Xen's
-- list of domains. You still need to give it some space, assign it some VCPUs,
-- map some pages, transfer the kernel, set up the CPU context, and so on. I
-- believe the domain is created in the Paused state.
createDomain :: Maybe DomId -> SID -> UUID -> Xen DomId
createDomain m_Dom ssid h = runDomCtlOp (#const CREATE_OP) dom datum
 where dom = fromMaybe (DomId 0) m_Dom
       datum = CreateData ssid h dom

-- |Destroy a running domain. 
destroyDomain :: DomId -> Xen ()
destroyDomain dom = runDomCtlOp (#const DESTROY_OP) dom datum
 where datum = DestroyData dom

-- |Pause a domain that's currently running.
pauseDomain :: DomId -> Xen ()
pauseDomain dom = runDomCtlOp (#const PAUSE_OP) dom datum
 where datum = PauseData dom

-- |Unpause a previously-paused domain.
unpauseDomain :: DomId -> Xen ()
unpauseDomain dom = runDomCtlOp (#const UNPAUSE_OP) dom datum
 where datum = UnpauseData dom

-- |Get basic information about the given domain. You may pass in 
-- 'Hypervisor.Basics.domidSelf' to get information about yourself without
-- knowing your domain id.
domainInfo :: DomId -> Xen DomainInfo
domainInfo dom = runDomCtlOp (#const DOMINFO_OP) dom datum
 where datum = DomInfoData dom

-- |Returns the current domain's id, but does it in a pretty reprehensible way.
-- You should probably use the related, non-evil function in the XenDevice
-- library.
evilBadMyDomId :: IO DomId
evilBadMyDomId = do
  di <- get_my_domid
  if di < 0
     then fail "Couldn't get own DomId. Something is really, really wrong."
     else return $ DomId $ fromIntegral di

-- |Fetch the entire processor context for the given VCPU of the given domain.
domainRegisterContext :: DomId -> VCPU -> Xen RegisterContext
domainRegisterContext dom (VCPU vcpu) = do
  buffer <- malloc
  bzero buffer (#size vcpu_guest_context_t)
  _ <- runDomCtlOp (#const GTREGCONT_OP) dom (GetRegContData dom vcpu buffer) `xOnException`
            free buffer
  realres <- peek buffer
  free buffer
  return realres

-- |Set the processor context for the given VCPU of the given domain.
setDomainRegisterContext :: DomId -> VCPU -> RegisterContext -> Xen ()
setDomainRegisterContext dom (VCPU vcpu) ctxt = do
  buffer <- malloc
  poke buffer ctxt
  res <- runDomCtlOp (#const STREGCONT_OP) dom (SetRegContData dom vcpu buffer)
  free buffer
  return res

-- |Set what physical processor a virtual processor runs on. I'm not sure how
-- strictly this is followed. It may simply preferentially schedule the VCPU on
-- the given CPU(s).
setDomainCPUAffinity :: DomId -> VCPU -> CPUMap -> Xen ()
setDomainCPUAffinity d (VCPU v) cmap = runDomCtlOp (#const STAFFIN_OP) d datum
 where datum = SetAffinData d v cmap

-- |Initialize the hypercall page inside the domain's page space. Note that this
-- routine takes a machine frame number rather than a virtual address.
hypercallInit :: DomId -> Word32 -> Xen ()
hypercallInit dom mfn = runDomCtlOp (#const HYPERINIT_OP) dom datum
 where datum = HyperInitData dom mfn

-- |Reset the domain's handle. I'm not sure why you'd want to use this, but here
-- it is.
setDomainHandle :: DomId -> UUID -> Xen ()
setDomainHandle dom uuid = runDomCtlOp (#const STDOMHNDL_OP) dom datum
 where datum = SetDomHndlData dom uuid

-- |Set the maximum number of virtual central processing units for a domain.
setDomainMaxVCPUs :: DomId -> Word32 -> Xen ()
setDomainMaxVCPUs dom maxvcpus = runDomCtlOp (#const MAXVCPUS_OP) dom datum
 where datum = MaxVCPUsData dom maxvcpus

-- |Get the current contents of the Xen emergency console ring. If passed
-- True, then the ring will be cleared after this operation.
readEmergencyConsole :: Bool -> Xen String
readEmergencyConsole clear = do
  -- currently the console is 16384 bytes long. If this changes, I'm going
  -- to be upset with myself ...
  buffer <- mallocBytes 16384
  let datum = ReadConData clear False 0 16384 buffer
  (_, count) <- runSysCtlOp (#const XEN_SYSCTL_readconsole) datum
  retval <- peekCStringLen (buffer, fromIntegral count)
  free buffer 
  return retval

-- |Get information about a domain's memory, storing it in the passed buffer. 
-- The routine takes the maximum number of entries the buffer can accept, and
-- returns the number of entries written. 
domainMemoryList :: DomId -> Word64 -> Ptr Word32 -> Xen Word32
domainMemoryList dom maxl buffer = runDomCtlOp (#const GTMLIST_OP) dom datum
 where datum = GetMemListData dom maxl buffer

-- |Get information about a given page frame within a domain. The page number is
-- a physical page frame number, not a virtual address.
domainPageFrameInfo :: DomId -> Word32 -> Xen [PageFlag]
domainPageFrameInfo dom mfn = runDomCtlOp (#const GTFRMINFO_OP) dom datum
 where datum = GetFrameData dom mfn


-- |Set the maximum amount of memory a domain can use. The amount should be
-- given in kilobytes.
setDomainMaxMemory :: DomId -> Word64 -> Xen ()
setDomainMaxMemory dom maxm = runDomCtlOp (#const STMAXMEM_OP) dom datum
 where datum = SetMaxMemData dom maxm

-- |Allocate the machine frames for a new foreign domain. This uses memory in
-- Xen's global free memory pool rather than your own memory space. Note, 
-- again, that the return value is a list of machine frame numbers, not a list
-- of machine addresses. The arguments are the domain to create the memory for
-- and the amount of memory (in KBytes) to give the new domain.
allocForeignMachineFrames :: DomId -> Word32 -> Xen [Word32]
allocForeignMachineFrames (DomId dom) maxMemKB = do
  let num_pages = maxMemKB `div` 4
      num_pagesI = fromIntegral num_pages
      pfn_list = [0 .. num_pages-1]
  ptr <- mallocArray num_pagesI
  pokeArray ptr pfn_list
  ppres <- populate_physmap dom num_pages ptr
  res <- if ppres == 0
           then peekArray num_pagesI ptr
           else xThrow $ toEnum $ fromIntegral (-ppres)
  free ptr
  return res

-- |A batched version of domainPageFrameInfo, which takes a list of machine
-- page frames to get information about, and returns a list of page attributes
-- for each of those pages.
domainPageFrameInfo2 :: DomId -> [Word32] -> Xen [[PageFlag]]
domainPageFrameInfo2 dom mfns = do
  ptr <- mallocBytes (length mfns * 4)
  pokeArray ptr mfns
  _ <- runDomCtlOp (#const GETPFI2_OP) dom (GetPFI2Data dom num ptr) `xOnException`
             free ptr
  wrds <- peekArray (length mfns) ptr
  free ptr
  return $ map parseFrameInfoFlags wrds
 where num = fromIntegral $ length mfns     

-- |I really have no clue what these do.
memtypeAdd :: Word32 -> Word64 -> Word32 -> Xen (Word32, Word32)
memtypeAdd mfn nr_mfns mtype = runPlatOp (#const MTYPE_ADD_OP) datum
 where datum = MTypeAddData mfn nr_mfns mtype

-- |I really have no clue what these do.
memtypeDelete :: Word32 -> Word32 -> Xen ()
memtypeDelete handle reg = runPlatOp (#const MTYPE_DEL_OP) datum
 where datum = MTypeDelData handle reg

-- |I really have no clue what these do.
memtypeRead :: Word32 -> Xen (Word32, Word64, Word32)
memtypeRead reg = runPlatOp (#const MTYPE_RD_OP) datum
 where datum = MTypeRdData reg

-- |Get information about the underlying, physical machine.
hostPhysicalInfo :: Xen HostPhysicalInfo
hostPhysicalInfo = runSysCtlOp (#const PHYSINFO_OP) PhysInfoData

-- |Either returns the id of the current scheduler, or the running domain's id
-- within the scheduler.
currentSchedulerId :: Xen Word32
currentSchedulerId = runSysCtlOp (#const SCHEDID_OP) SchedIdData

-- |Get basic information about a group of domains. The first argument is the 
-- domain to start with, the second is the number of domains after that you 
-- want to get information about.
listDomainInfo :: DomId -> Word32 -> Xen [DomainInfo]
listDomainInfo dom maxd = do
  buffer <- mallocArray (fromIntegral maxd)
  num <- runSysCtlOp (#const LDOMINFO_OP) (LDomInfoData dom maxd buffer) `xOnException`
             free buffer
  res' <- peekArray (fromIntegral num) buffer
  free buffer
  return res'

-- |Returns 'True' if the operations succeed, or false if there was a problem.
-- You should invoke this before performing any IO port actions, including
-- setting permissions.
initIOPorts :: IO Bool
initIOPorts = fmap (0 ==) init_io_bitmap


-- |Set whether or not the given domain is allowed to receive the given 
-- physical IRQ.
setIRQPermission :: DomId -> Word8 -> Bool -> Xen ()
setIRQPermission dom pirq allow = runDomCtlOp (#const STIRQPERM_OP) dom datum
 where datum = SetIRQPermData dom pirq (if allow then 1 else 0)


-- |Set whether or not the given domain is allowed to access the given IO 
-- ports. The arguments are the port and how many ports after that (inclusive)
-- the domain should or shouldn't be allowed to access
setIOPortPermission :: DomId -> Word32 -> Word32 -> Bool -> Xen ()
setIOPortPermission d f num allow = runDomCtlOp (#const STIOPPERM_OP) d datum
 where datum = SetIOPortData d f num (if allow then 1 else 0)

-- |Set whether or not the given domain is allowed to access the given IO 
-- memory. Note that the second argument is the physical page number of the 
-- memory (not the virtual address), and the third argument is how many pages
-- after that (inclusive) the domain should or should not be able to access.
#ifdef SPLIT_PRIVILEGED
setIOMemoryPermission :: DomId -> Word64 -> Word64 -> Bool -> Xen ()
#else
setIOMemoryPermission :: DomId -> Word32 -> Word64 -> Bool -> Xen ()
#endif
setIOMemoryPermission d f n allow = runDomCtlOp (#const STIOMPERM_OP) d datum
 where datum = SetIOMemData d f n (if allow then 1 else 0)

-- |Set the IO privilege level (IOPL) for the current VCPU.
setIOPrivilegeLevel :: Word32 -> Xen ()
setIOPrivilegeLevel x
  | x > 3     = xThrow EINVAL
  | otherwise = do res <- set_iopl x
                   if res == 0
                     then return ()
                     else xThrow $ toEnum (-res)

-- |Send an end of interrupt (EOI) message for the given IRQ
sendEOI :: Word32 -> Xen ()
sendEOI x = do
  res <- send_eoi x
  if res == 0
    then return ()
    else xThrow $ toEnum (-res)

-- |Update the virtual address mapping of another domain.
updateOthersVAMapping :: DomId -> Word32 -> Word32 -> Word32 -> Xen ()
updateOthersVAMapping (DomId dom) va new_val flags =
    do ret <- update_va_mapping_other va new_val flags dom
       if ret == 0
          then return ()
          else xThrow $ toEnum $ fromIntegral (-ret)

-- |Allocates an unbound port on behalf of a given domain. The first argument is
-- the domain for which to allocate the port, the second argument is the domain
-- that should be able to bind that port.
allocPortForDomain :: DomId -> DomId -> Xen Port
allocPortForDomain (DomId forDom) (DomId remoteDom) = do
  res <- evtchn_alloc_unbound forDom remoteDom
  case () of
             () | res < 0    -> xThrow $ toEnum $ fromIntegral (-res)
                | res < 1024 -> return $ toPort res
                | otherwise  -> error "INTERNAL ERROR: Bad port! (allocFor)"
--
-- --------------------------------------------------------------------------
--

bbit :: Bits a => Bool -> a -> a
bbit True a = a
bbit False _ = 0

parseFrameInfoFlags :: Word32 -> [PageFlag]
parseFrameInfoFlags flags = 
#ifdef SPLIT_PRIVILEGED
  let pinned = if (flags .&. (#const XEN_DOMCTL_PFINFO_LPINTAB)) /= 0
                  then [PagePinned]
                  else []
      tab = case flags .&. (#const XEN_DOMCTL_PFINFO_LTAB_MASK) of
              (#const XEN_DOMCTL_PFINFO_L1TAB) -> [Lvl1PageTable]
              (#const XEN_DOMCTL_PFINFO_L2TAB) -> [Lvl2PageTable]
              (#const XEN_DOMCTL_PFINFO_L3TAB) -> [Lvl3PageTable]
              (#const XEN_DOMCTL_PFINFO_L4TAB) -> [Lvl4PageTable]
              (#const XEN_DOMCTL_PFINFO_XTAB)  -> [PageInvalid]
              _                                -> []
  in (pinned ++ tab)
#else
  let pinned = if (flags .&. (#const LPINTAB)) /= 0 then [PagePinned] else []
      tab = case flags .&. (#const LTAB_MASK) of
              (#const L1TAB) -> [Lvl1PageTable]
              (#const L2TAB) -> [Lvl2PageTable]
              (#const L3TAB) -> [Lvl3PageTable]
              (#const L4TAB) -> [Lvl4PageTable]
              (#const XTAB)  -> [PageInvalid]
              _              -> []
  in (pinned ++ tab)
#endif

--
-- --------------------------------------------------------------------------
--

instance Storable RegisterContext where
  sizeOf _ = (#size vcpu_guest_context_t)
  alignment _ = 1
  peek ptr = do
    arch_specific <- peekArchSpecific ptr
    fpu_ctxt <- peekArray 512 ((#ptr vcpu_guest_context_t, fpu_ctxt) ptr)
    cpu_flags <- (#peek vcpu_guest_context_t, flags) ptr
    trap_ctxt <- peekArray 256 ((#ptr vcpu_guest_context_t, trap_ctxt) ptr)
    ldt_base <- (#peek vcpu_guest_context_t, ldt_base) ptr
    ldt_ents <- (#peek vcpu_guest_context_t, ldt_ents) ptr
    gdt_frames <- peekArray 16 ((#ptr vcpu_guest_context_t, gdt_frames) ptr)
    gdt_ents <- (#peek vcpu_guest_context_t, gdt_ents) ptr
    kernel_ss <- (#peek vcpu_guest_context_t, kernel_ss) ptr
    kernel_sp <- (#peek vcpu_guest_context_t, kernel_sp) ptr
    ctrlreg_list <- peekArray 8 ((#ptr vcpu_guest_context_t, ctrlreg) ptr)
    debugreg_list <- peekArray 8 ((#ptr vcpu_guest_context_t, debugreg) ptr)
    event_callback_eip <- (#peek vcpu_guest_context_t, event_callback_eip) ptr
    failsafe_callback_eip <- (#peek vcpu_guest_context_t, failsafe_callback_eip) ptr
    vm_assist <- (#peek vcpu_guest_context_t, vm_assist) ptr
    let [c0, c1, c2, c3, c4, c5, c6, c7] = ctrlreg_list
        ctrlreg = ControlRegisterSet c0 c1 c2 c3 c4 c5 c6 c7
    let [d0, d1, d2, d3, d4, d5, d6, d7] = debugreg_list
        debugreg = DebugRegisterSet d0 d1 d2 d3 d4 d5 d6 d7
    return RegisterContext {..}

  poke ptr gc = do 
    () <- assert ((length (fpu_ctxt gc)) <= 512) $ return ()
    () <- assert ((length (trap_ctxt gc)) <= 256) $ return ()
    () <- assert ((length (gdt_frames gc)) <= 16) $ return ()
    pokeArchSpecific ptr (arch_specific gc)
    pokeArray ((#ptr vcpu_guest_context_t, fpu_ctxt) ptr)
              (fpu_ctxt gc)
    (#poke vcpu_guest_context_t, flags) ptr (cpu_flags gc)
    pokeArray ((#ptr vcpu_guest_context_t, trap_ctxt) ptr)
              (trap_ctxt gc)
    (#poke vcpu_guest_context_t, ldt_base) ptr (ldt_base gc)
    (#poke vcpu_guest_context_t, ldt_ents) ptr (ldt_ents gc)
    pokeArray ((#ptr vcpu_guest_context_t, gdt_frames) ptr)
              (gdt_frames gc)
    (#poke vcpu_guest_context_t, gdt_ents) ptr (gdt_ents gc)
    (#poke vcpu_guest_context_t, kernel_ss) ptr (kernel_ss gc)
    (#poke vcpu_guest_context_t, kernel_sp) ptr (kernel_sp gc)
    let ControlRegisterSet c0 c1 c2 c3 c4 c5 c6 c7 = ctrlreg gc
        ctrlreg' = [c0, c1, c2, c3, c4, c5, c6, c7]
    pokeArray ((#ptr vcpu_guest_context_t, ctrlreg) ptr) ctrlreg'
    let DebugRegisterSet d0 d1 d2 d3 d4 d5 d6 d7 = debugreg gc
        dbgreg' = [d0, d1, d2, d3, d4, d5, d6, d7]
    pokeArray ((#ptr vcpu_guest_context_t, debugreg) ptr) dbgreg'
    (#poke vcpu_guest_context_t, event_callback_eip) ptr 
          (event_callback_eip gc)
    (#poke vcpu_guest_context_t, failsafe_callback_eip) ptr 
          (failsafe_callback_eip gc)
    (#poke vcpu_guest_context_t, vm_assist) ptr (vm_assist gc)

instance Storable TrapInfo where
  sizeOf _ = (#size trap_info_t)
  alignment _ = 1
  peek ptr = do vec <- (#peek trap_info_t, vector) ptr
                tfs <- (#peek trap_info_t, flags) ptr
                tcs <- (#peek trap_info_t, cs) ptr
                add <- (#peek trap_info_t, address) ptr
                return $ TrapInfo vec tfs tcs add
  poke ptr ti = do (#poke trap_info_t, vector) ptr (vector ti)
                   (#poke trap_info_t, flags) ptr (trap_flags ti)
                   (#poke trap_info_t, cs) ptr (trap_cs ti)
                   (#poke trap_info_t, address) ptr (address ti)

#ifdef SPLIT_PRIVILEGED
instance Storable DomainInfo where
  sizeOf _ = (#size xen_domctl_getdomaininfo_t)
  alignment _ = 1
  peek ptr = do 
    dom <- (#peek xen_domctl_getdomaininfo_t, domain) ptr
    flags <- (#peek xen_domctl_getdomaininfo_t, flags) ptr :: IO Word32
    totps <- (#peek xen_domctl_getdomaininfo_t, tot_pages) ptr
    maxps <- (#peek xen_domctl_getdomaininfo_t, max_pages) ptr
    shr_pages <- (#peek xen_domctl_getdomaininfo_t, shr_pages) ptr
    si_frame <- (#peek xen_domctl_getdomaininfo_t, shared_info_frame) ptr
    cputime <- (#peek xen_domctl_getdomaininfo_t, cpu_time) ptr
    nocpus <- (#peek xen_domctl_getdomaininfo_t, nr_online_vcpus) ptr
    maxid <- (#peek xen_domctl_getdomaininfo_t, max_vcpu_id) ptr
    ssid <- (#peek xen_domctl_getdomaininfo_t, ssidref) ptr
    handle <- peekArray 16 ((#ptr xen_domctl_getdomaininfo_t, handle) ptr)
    return $ DomainInfo { 
      domain = DomId dom
#ifdef XEN_DOMINF_dying
    , dying     = 0 /= flags .&. (#const XEN_DOMINF_dying)
    , hvm_guest = 0 /= flags .&. (#const XEN_DOMINF_hvm_guest)
    , shutdown  = 0 /= flags .&. (#const XEN_DOMINF_shutdown)
    , paused    = 0 /= flags .&. (#const XEN_DOMINF_paused)
    , blocked   = 0 /= flags .&. (#const XEN_DOMINF_blocked)
    , running   = 0 /= flags .&. (#const XEN_DOMINF_running)
#ifdef XEN_DOMINF_cpushift
    , cpu       = fromIntegral
                $ shiftR flags (#const XEN_DOMINF_cpushift) .&. 
                    (#const XEN_DOMINF_cpumask)
#endif
    , shutdown_code = fromIntegral
                    $ shiftR flags (#const XEN_DOMINF_shutdownshift) .&. 
                        (#const XEN_DOMINF_shutdownmask)
#else
    , dying     = 0 /= flags .&. (#const DOMFLAGS_DYING)
    , hvm_guest = False
    , shutdown  = 0 /= flags .&. (#const DOMFLAGS_SHUTDOWN)
    , paused    = 0 /= flags .&. (#const DOMFLAGS_PAUSED)
    , blocked   = 0 /= flags .&. (#const DOMFLAGS_BLOCKED)
    , running   = 0 /= flags .&. (#const DOMFLAGS_RUNNING)
    , cpu       = fromIntegral
                $ shiftR flags (#const DOMFLAGS_CPUSHIFT) .&. 
                    (#const DOMFLAGS_CPUMASK)
    , shutdown_code = fromIntegral
                    $ shiftR flags (#const DOMFLAGS_SHUTDOWNSHIFT) .&. 
                        (#const DOMFLAGS_SHUTDOWNMASK)
#endif
    , tot_pages = totps
    , max_pages = maxps
    , shr_pages = shr_pages
    , shared_info_frame = si_frame
    , cpu_time = cputime
    , nr_online_vcpus = nocpus
    , max_vcpu_id = maxid
    , ssidref = ssid
    , domain_handle = handle
    }
  poke ptr di = do 
    let DomId dom = (domain di)
#ifdef XEN_DOMINF_dying
        flags :: Word32
        flags = bbit (dying di) (#const XEN_DOMINF_dying) .|.
                bbit (hvm_guest di) (#const XEN_DOMINF_hvm_guest) .|.
                bbit (shutdown di) (#const XEN_DOMINF_shutdown) .|.
                bbit (paused di) (#const XEN_DOMINF_paused) .|.
                bbit (blocked di) (#const XEN_DOMINF_blocked) .|.
                bbit (running di) (#const XEN_DOMINF_running) .|.
#ifdef XEN_DOMINF_cpushift
                shiftL (fromIntegral (cpu di)) (#const XEN_DOMINF_cpushift) .|.
#endif
                shiftL (fromIntegral (shutdown_code di)) (#const XEN_DOMINF_shutdownshift)
#else
        flags = bbit (dying di) (#const DOMFLAGS_DYING) .|.
                bbit (shutdown di) (#const DOMFLAGS_SHUTDOWN) .|.
                bbit (paused di) (#const DOMFLAGS_PAUSED) .|.
                bbit (blocked di) (#const DOMFLAGS_BLOCKED) .|.
                bbit (running di) (#const DOMFLAGS_RUNNING) .|.
                shiftL (fromIntegral (cpu di)) (#const DOMFLAGS_CPUSHIFT) .|.
                shiftL (fromIntegral (shutdown_code di)) (#const DOMFLAGS_SHUTDOWNSHIFT)
#endif
    (#poke xen_domctl_getdomaininfo_t, domain) ptr dom
    (#poke xen_domctl_getdomaininfo_t, flags) ptr flags
    (#poke xen_domctl_getdomaininfo_t, tot_pages) ptr (tot_pages di)
    (#poke xen_domctl_getdomaininfo_t, max_pages) ptr (max_pages di)
    (#poke xen_domctl_getdomaininfo_t, shr_pages) ptr (shr_pages di)
    (#poke xen_domctl_getdomaininfo_t, shared_info_frame) ptr 
        (shared_info_frame di)
    (#poke xen_domctl_getdomaininfo_t, cpu_time) ptr (cpu_time di)
    (#poke xen_domctl_getdomaininfo_t, nr_online_vcpus) ptr (nr_online_vcpus di)
    (#poke xen_domctl_getdomaininfo_t, max_vcpu_id) ptr (max_vcpu_id di)
    (#poke xen_domctl_getdomaininfo_t, ssidref) ptr (ssidref di)
    pokeArray ((#ptr xen_domctl_getdomaininfo_t, handle) ptr) 
        (domain_handle di)
#else
instance Storable DomainInfo where
  sizeOf _ = (#size dom0_getdomaininfo_t)
  alignment _ = 1
  peek ptr = do 
    dom <- (#peek dom0_getdomaininfo_t, domain) ptr
    flags <- (#peek dom0_getdomaininfo_t, flags) ptr :: IO Word32
    totpages <- (#peek dom0_getdomaininfo_t, tot_pages) ptr
    maxpages <- (#peek dom0_getdomaininfo_t, max_pages) ptr
    shr_pages <- (#peek dom0_getdomaininfo_t, shr_pages) ptr
    si_frame <- (#peek dom0_getdomaininfo_t, shared_info_frame) ptr
    cputime <- (#peek dom0_getdomaininfo_t, cpu_time) ptr
    nocpus <- (#peek dom0_getdomaininfo_t, nr_online_vcpus) ptr
    maxvcpuid <- (#peek dom0_getdomaininfo_t, max_vcpu_id) ptr
    ssid <- (#peek dom0_getdomaininfo_t, ssidref) ptr
    handle <- peekArray 16 ((#ptr dom0_getdomaininfo_t,handle) ptr)
    return $ DomainInfo { 
      domain = DomId dom
    , dying     = 0 /= flags .&. (#const DOMFLAGS_DYING)
    , hvm_guest = False
    , shutdown  = 0 /= flags .&. (#const DOMFLAGS_SHUTDOWN)
    , paused    = 0 /= flags .&. (#const DOMFLAGS_PAUSED)
    , blocked   = 0 /= flags .&. (#const DOMFLAGS_BLOCKED)
    , running   = 0 /= flags .&. (#const DOMFLAGS_RUNNING)
    , cpu       = shiftR flags (#const DOMFLAGS_CPUSHIFT) .&. 
                    (#const DOMFLAGS_CPUMASK)
    , shutdown_code = shiftR flags (#const DOMFLAGS_SHUTDOWNSHIFT) .&. 
                    (#const DOMFLAGS_SHUTDOWNMASK)
    , tot_pages = totpages
    , max_pages = maxpages
    , shr_pages = shr_pages
    , shared_info_frame = si_frame
    , cpu_time = cputime
    , nr_online_vcpus = nocpus
    , max_vcpu_id = maxvcpuid
    , ssidref = ssid
    , domain_handle = handle
    }
  poke ptr di = do 
    let DomId dom = (domain di)
        flags = bbit (dying di) (#const DOMFLAGS_DYING) .|.
                bbit (shutdown di) (#const DOMFLAGS_SHUTDOWN) .|.
                bbit (paused di) (#const DOMFLAGS_PAUSED) .|.
                bbit (blocked di) (#const DOMFLAGS_BLOCKED) .|.
                bbit (running di) (#const DOMFLAGS_RUNNING) .|.
                shiftL (cpu di) (#const DOMFLAGS_CPUSHIFT) .|.
                shiftL (shutdown_code di) (#const DOMFLAGS_SHUTDOWNSHIFT)
    (#poke dom0_getdomaininfo_t, domain) ptr dom
    (#poke dom0_getdomaininfo_t, flags) ptr flags
    (#poke dom0_getdomaininfo_t, tot_pages) ptr (tot_pages di)
    (#poke dom0_getdomaininfo_t, max_pages) ptr (max_pages di)
    (#poke dom0_getdomaininfo_t, shr_pages) ptr (shr_pages di)
    (#poke dom0_getdomaininfo_t, shared_info_frame) ptr (shared_info_frame di)
    (#poke dom0_getdomaininfo_t, cpu_time) ptr (cpu_time di)
    (#poke dom0_getdomaininfo_t, nr_online_vcpus) ptr (nr_online_vcpus di)
    (#poke dom0_getdomaininfo_t, max_vcpu_id) ptr (max_vcpu_id di)
    (#poke dom0_getdomaininfo_t, ssidref) ptr (ssidref di)
    pokeArray ((#ptr dom0_getdomaininfo_t, handle) ptr)
              (domain_handle di)
#endif

instance Storable HostPhysicalInfo where
  alignment _ = 8
  sizeOf _ = (#size xen_sysctl_physinfo_t)
  peek ptr = do
    threads_per_core <- (#peek xen_sysctl_physinfo_t, threads_per_core) ptr
    cores_per_socket <- (#peek xen_sysctl_physinfo_t, cores_per_socket) ptr
    num_cpus         <- (#peek xen_sysctl_physinfo_t, nr_cpus         ) ptr
    max_node_id      <- (#peek xen_sysctl_physinfo_t, max_node_id     ) ptr
    cpu_khz          <- (#peek xen_sysctl_physinfo_t, cpu_khz         ) ptr
    total_pages      <- (#peek xen_sysctl_physinfo_t, total_pages     ) ptr
    free_pages       <- (#peek xen_sysctl_physinfo_t, free_pages      ) ptr
    scrub_pages      <- (#peek xen_sysctl_physinfo_t, scrub_pages     ) ptr
    hw_capability    <- peekArray 8 ((#ptr xen_sysctl_physinfo_t, hw_cap) ptr)
    max_cpu_id       <- (#peek xen_sysctl_physinfo_t, max_cpu_id      ) ptr
#if XEN_SYSCTL_INTERFACE_VERSION < 0x00000008
    cpu_to_node      <- (#peek xen_sysctl_physinfo_t, cpu_to_node     ) ptr
#endif
    capabilities     <- (#peek xen_sysctl_physinfo_t, capabilities    ) ptr
    return HostPhysicalInfo {..}
  poke ptr hpi = do
    (#poke xen_sysctl_physinfo_t, threads_per_core) ptr (threads_per_core hpi)
    (#poke xen_sysctl_physinfo_t, cores_per_socket) ptr (cores_per_socket hpi)
    (#poke xen_sysctl_physinfo_t, nr_cpus         ) ptr (num_cpus         hpi)
    (#poke xen_sysctl_physinfo_t, max_node_id     ) ptr (max_node_id      hpi)
    (#poke xen_sysctl_physinfo_t, cpu_khz         ) ptr (cpu_khz          hpi)
    (#poke xen_sysctl_physinfo_t, total_pages     ) ptr (total_pages      hpi)
    (#poke xen_sysctl_physinfo_t, free_pages      ) ptr (free_pages       hpi)
    (#poke xen_sysctl_physinfo_t, scrub_pages     ) ptr (scrub_pages      hpi)
    pokeArray ((#ptr xen_sysctl_physinfo_t, hw_cap) ptr) (hw_capability   hpi)
    (#poke xen_sysctl_physinfo_t, max_cpu_id      ) ptr (max_cpu_id       hpi)
#if XEN_SYSCTL_INTERFACE_VERSION < 0x00000008
    (#poke xen_sysctl_physinfo_t, cpu_to_node     ) ptr (cpu_to_node      hpi)
#endif
    (#poke xen_sysctl_physinfo_t, capabilities    ) ptr (capabilities     hpi)

--
-- --------------------------------------------------------------------------
--

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word32 -> IO ()

foreign import ccall unsafe "privileged.h init_io_bitmap"
  init_io_bitmap :: IO Int
foreign import ccall unsafe "privileged.h update_va_mapping_otherdomain" 
  update_va_mapping_other :: Word32 -> Word32 -> Word32 -> Word16 -> IO Int
foreign import ccall unsafe "privileged.h populate_physmap" 
  populate_physmap :: Word16 -> Word32 -> Ptr a -> IO Int
foreign import ccall unsafe "privileged.h get_my_domid"
  get_my_domid :: IO Int

foreign import ccall unsafe "events.h evtchn_alloc_unbound" 
  evtchn_alloc_unbound :: Word16 -> Word16 -> IO Word10

#ifdef SPLIT_PRIVILEGED
foreign import ccall unsafe "privileged.h do_domctl_op" 
  do_domctl_op :: Ptr a -> IO Int
foreign import ccall unsafe "privileged.h do_sysctl_op" 
  do_sysctl_op :: Ptr a -> IO Int
foreign import ccall unsafe "privileged.h do_platform_op" 
  do_platform_op :: Ptr a -> IO Int
#else
foreign import ccall unsafe "privileged.h do_dom0_operation" 
  do_dom0_op :: Ptr a -> IO Int
#endif

foreign import ccall unsafe "privileged.h set_iopl"
  set_iopl :: Word32 -> IO Int
foreign import ccall unsafe "privileged.h send_eoi"
  send_eoi :: Word32 -> IO Int

