-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |The low-level host info structure, lifted to Haskell.
module Hypervisor.Structures.PhysicalInfo(
         HostPhysicalInfo(..)
       , XenCapability(..)
       , X86Capability(..)
       , featureMap -- Exported mostly for testing
       )
 where

import Data.Bits
import Data.Tuple
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

#define __XEN__
#include <stdint.h>
#include <sys/types.h>
#include <xen/sysctl.h>

data HostPhysicalInfo = HostPhysicalInfo {
    hpiThreadsPerCore  :: Word32
  , hpiCoresPerSocket  :: Word32
  , hpiNumberOfCPUs    :: Word32 -- ^This is the # of CPUs currently online
  , hpiMaxCPUId        :: Word32
  , hpiNumberOfNodes   :: Word32 -- ^Again, online
  , hpiMaxNodeId       :: Word32
  , hpiCPUKHz          :: Word32
  , hpiTotalPages      :: Word64
  , hpiFreePages       :: Word64
  , hpiScribPages      :: Word64
  , hpiHWCapabilities  :: [X86Capability]
  , hpiXenCapabilities :: [XenCapability]
  }
 deriving (Eq,Show,Generic)

instance Storable HostPhysicalInfo where
  sizeOf _    = (#size xen_sysctl_physinfo_t)
  alignment _ = 8
  peek ptr    = do
    tpc <- (#peek xen_sysctl_physinfo_t,threads_per_core) ptr
    cps <- (#peek xen_sysctl_physinfo_t,cores_per_socket) ptr
    ncp <- (#peek xen_sysctl_physinfo_t,nr_cpus)          ptr
    mcp <- (#peek xen_sysctl_physinfo_t,max_cpu_id)       ptr
    nno <- (#peek xen_sysctl_physinfo_t,nr_nodes)         ptr
    mno <- (#peek xen_sysctl_physinfo_t,max_node_id)      ptr
    khz <- (#peek xen_sysctl_physinfo_t,cpu_khz)          ptr
    top <- (#peek xen_sysctl_physinfo_t,total_pages)      ptr
    frp <- (#peek xen_sysctl_physinfo_t,free_pages)       ptr
    scp <- (#peek xen_sysctl_physinfo_t,scrub_pages)      ptr
    let capOff   = (#offset xen_sysctl_physinfo_t,hw_cap)
        capStart = castPtr (ptr `plusPtr` capOff)
    cap <- parseX86Capabilities `fmap` peekArray 8 capStart
    xcn <- (#peek xen_sysctl_physinfo_t,capabilities)     ptr
    let xcp = parseXenCapabilities xcn
    return (HostPhysicalInfo tpc cps ncp mcp nno mno khz top frp scp cap xcp)
  poke ptr x  = do
    (#poke xen_sysctl_physinfo_t,threads_per_core) ptr (hpiThreadsPerCore x)
    (#poke xen_sysctl_physinfo_t,cores_per_socket) ptr (hpiCoresPerSocket x)
    (#poke xen_sysctl_physinfo_t,nr_cpus)          ptr (hpiNumberOfCPUs x)
    (#poke xen_sysctl_physinfo_t,max_cpu_id)       ptr (hpiMaxCPUId x)
    (#poke xen_sysctl_physinfo_t,nr_nodes)         ptr (hpiNumberOfNodes x)
    (#poke xen_sysctl_physinfo_t,max_node_id)      ptr (hpiMaxNodeId x)
    (#poke xen_sysctl_physinfo_t,cpu_khz)          ptr (hpiCPUKHz x)
    (#poke xen_sysctl_physinfo_t,total_pages)      ptr (hpiTotalPages x)
    (#poke xen_sysctl_physinfo_t,free_pages)       ptr (hpiFreePages x)
    (#poke xen_sysctl_physinfo_t,scrub_pages)      ptr (hpiScribPages x)
    let capOff   = (#offset xen_sysctl_physinfo_t,hw_cap)
        capStart = castPtr (ptr `plusPtr` capOff)
    pokeArray capStart (renderX86Capabilities (hpiHWCapabilities x))
    let xcp      = renderXenCapabilities (hpiXenCapabilities x)
    (#poke xen_sysctl_physinfo_t,capabilities) ptr xcp

-- ----------------------------------------------------------------------------

data XenCapability = PlatformSupportsHVM | PlatformSupportsDirectIO
 deriving (Eq,Show,Generic)

parseXenCapabilities :: Word32 -> [XenCapability]
parseXenCapabilities x = hvm ++ dio
 where
  hvm = mask (#const XEN_SYSCTL_PHYSCAP_hvm)      PlatformSupportsHVM
  dio = mask (#const XEN_SYSCTL_PHYSCAP_hvm_directio) PlatformSupportsDirectIO
  mask c v = if x .&. c /= 0 then [v] else []

renderXenCapabilities :: [XenCapability] -> Word32
renderXenCapabilities x = hvm .|. dio
 where
  hvm = unmask PlatformSupportsHVM      (#const XEN_SYSCTL_PHYSCAP_hvm)
  dio = unmask PlatformSupportsDirectIO (#const XEN_SYSCTL_PHYSCAP_hvm_directio)
  unmask v c = if v `elem` x then c else 0

-- ----------------------------------------------------------------------------

parseX86Capabilities :: [Word32] -> [X86Capability]
parseX86Capabilities xs = concat $ zipWith translateWord xs [0,32..]
 where
  translateWord x off   = concat (map (translateBit x off) [0..31])
  translateBit  x off b
    | testBit x b = case lookup (off + b) revFeatureMap of
                      Just c  -> [c]
                      Nothing -> []
    | otherwise   = []

renderX86Capabilities :: [X86Capability] -> [Word32]
renderX86Capabilities caps = foldl addFlag (replicate 8 0) caps
 where
  addFlag res flag =
    case lookup flag featureMap of
      Just n  -> setFlag res n
      Nothing -> res
  --
  setFlag []       _ = []
  setFlag (x:rest) n
   | n < 32          = setBit x n : rest
   | otherwise       = x : setFlag rest (n - 32)

data X86Capability =
    Feat_FPU -- ^Onboard FPU
  | Feat_VME -- ^Virtual Mode Extensions
  | Feat_DE -- ^Debugging Extensions
  | Feat_PSE -- ^Page Size Extensions
  | Feat_TSC -- ^Time Stamp Counter
  | Feat_MSR -- ^Model-Specific Registers, RDMSR, WRMSR
  | Feat_PAE -- ^Physical Address Extensions
  | Feat_MCE -- ^Machine Check Architecture
  | Feat_CX8 -- ^CMPXCHG8 instruction
  | Feat_APIC -- ^Onboard APIC
  | Feat_SEP -- ^SYSENTER/SYSEXIT
  | Feat_MTRR -- ^Memory Type Range Registers
  | Feat_PGE -- ^Page Global Enable
  | Feat_MCA -- ^Machine Check Architecture
  | Feat_CMOV -- ^CMOV instruction (FCMOVCC and FCOMI too if FPU present)
  | Feat_PAT -- ^Page Attribute Table
  | Feat_PSE36 -- ^36-bit PSEs
  | Feat_PN -- ^Processor serial number
  | Feat_CLFLSH -- ^Supports the CLFLUSH instruction
  | Feat_DS -- ^Debug Store
  | Feat_ACPI -- ^ACPI via MSR
  | Feat_MMX -- ^Multimedia Extensions
  | Feat_FXSR -- ^FXSAVE and FXRSTOR instructions (fast save and restore
  | Feat_XMM -- ^Streaming SIMD Extensions
  | Feat_XMM2 -- ^Streaming SIMD Extensions-2
  | Feat_SELFSNOOP -- ^CPU self snoop
  | Feat_HT -- ^Hyper-Threading
  | Feat_ACC -- ^Automatic clock control
  | Feat_IA64 -- ^IA-64 processor
  | Feat_PBE -- ^Pending Break Enable
  | Feat_SYSCALL -- ^SYSCALL/SYSRET
  | Feat_MP -- ^MP Capable.
  | Feat_NX -- ^Execute Disable
  | Feat_MMXEXT -- ^AMD MMX extensions
  | Feat_FFXSR -- ^FFXSR instruction optimizations
  | Feat_PAGE1GB -- ^1Gb large page support
  | Feat_RDTSCP -- ^RDTSCP
  | Feat_LM -- ^Long Mode (x86-64)
  | Feat_3DNOWEXT -- ^AMD 3DNow! extensions
  | Feat_3DNOW -- ^3DNow!
  | Feat_RECOVERY -- ^CPU in recovery mode
  | Feat_LONGRUN -- ^Longrun power control
  | Feat_LRTI -- ^LongRun table interface
  | Feat_CXMMX -- ^Cyrix MMX extensions
  | Feat_K6_MTRR -- ^AMD K6 nonstandard MTRRs
  | Feat_CYRIX_ARR -- ^Cyrix ARRs (= MTRRs)
  | Feat_CENTAUR_MCR -- ^Centaur MCRs (= MTRRs)
  | Feat_K8 -- ^Opteron, Athlon64
  | Feat_K7 -- ^Athlon
  | Feat_P3 -- ^P3
  | Feat_P4 -- ^P4
  | Feat_CONSTANT_TSC -- ^TSC ticks at a constant rate
  | Feat_NONSTOP_TSC -- ^TSC does not stop in C states
  | Feat_ARAT -- ^Always running APIC timer
  | Feat_ARCH_PERFMON -- ^Intel Architectural PerfMon
  | Feat_TSC_RELIABLE -- ^TSC is known to be reliable
  | Feat_XTOPOLOGY -- ^cpu topology enum extensions
  | Feat_CPUID_FAULTING -- ^cpuid faulting
  | Feat_XMM3 -- ^Streaming SIMD Extensions-3
  | Feat_PCLMULQDQ -- ^Carry-less mulitplication
  | Feat_DTES64 -- ^64-bit Debug Store
  | Feat_MWAIT -- ^Monitor/Mwait support
  | Feat_DSCPL -- ^CPL Qualified Debug Store
  | Feat_VMXE -- ^Virtual Machine Extensions
  | Feat_SMXE -- ^Safer Mode Extensions
  | Feat_EST -- ^Enhanced SpeedStep
  | Feat_TM2 -- ^Thermal Monitor 2
  | Feat_SSSE3 -- ^Supplemental Streaming SIMD Extensions-3
  | Feat_CID -- ^Context ID
  | Feat_FMA -- ^Fused Multiply Add
  | Feat_CX16 -- ^CMPXCHG16B
  | Feat_XTPR -- ^Send Task Priority Messages
  | Feat_PDCM -- ^Perf/Debug Capability MSR
  | Feat_PCID -- ^Process Context ID
  | Feat_DCA -- ^Direct Cache Access
  | Feat_SSE4_1 -- ^Streaming SIMD Extensions 4.1
  | Feat_SSE4_2 -- ^Streaming SIMD Extensions 4.2
  | Feat_X2APIC -- ^Extended xAPIC
  | Feat_MOVBE -- ^movbe instruction
  | Feat_POPCNT -- ^POPCNT instruction
  | Feat_TSC_DEADLINE -- ^"tdt" TSC Deadline Timer
  | Feat_AES -- ^AES instructions
  | Feat_XSAVE -- ^XSAVE/XRSTOR/XSETBV/XGETBV
  | Feat_OSXSAVE -- ^OSXSAVE
  | Feat_AVX -- ^Advanced Vector Extensions
  | Feat_F16C -- ^Half-precision convert instruction
  | Feat_RDRAND -- ^Digital Random Number Generator
  | Feat_HYPERVISOR -- ^Running under some hypervisor
  | Feat_XSTORE -- ^on-CPU RNG present (xstore insn)
  | Feat_XSTORE_EN -- ^on-CPU RNG enabled
  | Feat_XCRYPT -- ^on-CPU crypto (xcrypt insn)
  | Feat_XCRYPT_EN -- ^on-CPU crypto enabled
  | Feat_ACE2 -- ^Advanced Cryptography Engine v2
  | Feat_ACE2_EN -- ^ACE v2 enabled
  | Feat_PHE -- ^PadLock Hash Engine
  | Feat_PHE_EN -- ^PHE enabled
  | Feat_PMM -- ^PadLock Montgomery Multiplier
  | Feat_PMM_EN -- ^PMM enabled
  | Feat_LAHF_LM -- ^LAHF/SAHF in long mode
  | Feat_CMP_LEGACY -- ^If yes HyperThreading not valid
  | Feat_SVM -- ^Secure virtual machine
  | Feat_EXTAPIC -- ^Extended APIC space
  | Feat_CR8_LEGACY -- ^CR8 in 32-bit mode
  | Feat_ABM -- ^Advanced bit manipulation
  | Feat_SSE4A -- ^SSE-4A
  | Feat_MISALIGNSSE -- ^Misaligned SSE mode
  | Feat_3DNOWPREFETCH -- ^3DNow prefetch instructions
  | Feat_OSVW -- ^OS Visible Workaround
  | Feat_IBS -- ^Instruction Based Sampling
  | Feat_XOP -- ^extended AVX instructions
  | Feat_SKINIT -- ^SKINIT/STGI instructions
  | Feat_WDT -- ^Watchdog timer
  | Feat_LWP -- ^Light Weight Profiling
  | Feat_FMA4 -- ^4 operands MAC instructions
  | Feat_NODEID_MSR -- ^NodeId MSR
  | Feat_TBM -- ^trailing bit manipulations
  | Feat_TOPOEXT -- ^topology extensions CPUID leafs
  | Feat_FSGSBASE -- ^{RD,WR}{FS,GS}BASE instructions
  | Feat_BMI1 -- ^1st bit manipulation extensions
  | Feat_HLE -- ^Hardware Lock Elision
  | Feat_AVX2 -- ^AVX2 instructions
  | Feat_SMEP -- ^Supervisor Mode Execution Protection
  | Feat_BMI2 -- ^2nd bit manipulation extensions
  | Feat_ERMS -- ^Enhanced REP MOVSB/STOSB
  | Feat_INVPCID -- ^Invalidate Process Context ID
  | Feat_RTM -- ^Restricted Transactional Memory
 deriving (Eq,Show,Generic)

revFeatureMap :: [(Int, X86Capability)]
revFeatureMap  = map swap featureMap

featureMap :: [(X86Capability, Int)]
featureMap = [
    (Feat_FPU,    (0*32+ 0))
  , (Feat_VME,    (0*32+ 1))
  , (Feat_DE,    (0*32+ 2))
  , (Feat_PSE,   (0*32+ 3))
  , (Feat_TSC,    (0*32+ 4))
  , (Feat_MSR,    (0*32+ 5))
  , (Feat_PAE,    (0*32+ 6))
  , (Feat_MCE,    (0*32+ 7))
  , (Feat_CX8,    (0*32+ 8))
  , (Feat_APIC,  (0*32+ 9))
  , (Feat_SEP,    (0*32+11))
  , (Feat_MTRR,  (0*32+12))
  , (Feat_PGE,    (0*32+13))
  , (Feat_MCA,    (0*32+14))
  , (Feat_CMOV,  (0*32+15))
  , (Feat_PAT,    (0*32+16))
  , (Feat_PSE36,  (0*32+17))
  , (Feat_PN,    (0*32+18))
  , (Feat_CLFLSH,  (0*32+19))
  , (Feat_DS,    (0*32+21))
  , (Feat_ACPI,  (0*32+22))
  , (Feat_MMX,    (0*32+23))
  , (Feat_FXSR,  (0*32+24))
  , (Feat_XMM,    (0*32+25))
  , (Feat_XMM2,  (0*32+26))
  , (Feat_SELFSNOOP,  (0*32+27))
  , (Feat_HT,    (0*32+28))
  , (Feat_ACC,    (0*32+29))
  , (Feat_IA64,  (0*32+30))
  , (Feat_PBE,    (0*32+31))
  , (Feat_SYSCALL,  (1*32+11))
  , (Feat_MP,    (1*32+19))
  , (Feat_NX,    (1*32+20))
  , (Feat_MMXEXT,  (1*32+22))
  , (Feat_FFXSR,       (1*32+25))
  , (Feat_PAGE1GB,  (1*32+26))
  , (Feat_RDTSCP,  (1*32+27))
  , (Feat_LM,    (1*32+29))
  , (Feat_3DNOWEXT,  (1*32+30))
  , (Feat_3DNOW,  (1*32+31))
  , (Feat_RECOVERY,  (2*32+ 0))
  , (Feat_LONGRUN,  (2*32+ 1))
  , (Feat_LRTI,  (2*32+ 3))
  , (Feat_CXMMX,  (3*32+ 0))
  , (Feat_K6_MTRR,  (3*32+ 1))
  , (Feat_CYRIX_ARR,  (3*32+ 2))
  , (Feat_CENTAUR_MCR,  (3*32+ 3))
  , (Feat_K8,    (3*32+ 4))
  , (Feat_K7,    (3*32+ 5))
  , (Feat_P3,    (3*32+ 6))
  , (Feat_P4,    (3*32+ 7))
  , (Feat_CONSTANT_TSC, (3*32+ 8))
  , (Feat_NONSTOP_TSC,  (3*32+ 9))
  , (Feat_ARAT,  (3*32+ 10))
  , (Feat_ARCH_PERFMON, (3*32+11))
  , (Feat_TSC_RELIABLE, (3*32+12))
  , (Feat_XTOPOLOGY,    (3*32+13))
  , (Feat_CPUID_FAULTING, (3*32+14))
  , (Feat_XMM3,  (4*32+ 0))
  , (Feat_PCLMULQDQ,  (4*32+ 1))
  , (Feat_DTES64,  (4*32+ 2))
  , (Feat_MWAIT,  (4*32+ 3))
  , (Feat_DSCPL,  (4*32+ 4))
  , (Feat_VMXE,  (4*32+ 5))
  , (Feat_SMXE,  (4*32+ 6))
  , (Feat_EST,    (4*32+ 7))
  , (Feat_TM2,    (4*32+ 8))
  , (Feat_SSSE3,  (4*32+ 9))
  , (Feat_CID,    (4*32+10))
  , (Feat_FMA,    (4*32+12))
  , (Feat_CX16,        (4*32+13))
  , (Feat_XTPR,  (4*32+14))
  , (Feat_PDCM,  (4*32+15))
  , (Feat_PCID,  (4*32+17))
  , (Feat_DCA,    (4*32+18))
  , (Feat_SSE4_1,  (4*32+19))
  , (Feat_SSE4_2,  (4*32+20))
  , (Feat_X2APIC,  (4*32+21))
  , (Feat_MOVBE,  (4*32+22))
  , (Feat_POPCNT,  (4*32+23))
  , (Feat_TSC_DEADLINE, (4*32+24))
  , (Feat_AES,    (4*32+25))
  , (Feat_XSAVE,  (4*32+26))
  , (Feat_OSXSAVE,  (4*32+27))
  , (Feat_AVX,   (4*32+28))
  , (Feat_F16C,   (4*32+29))
  , (Feat_RDRAND,   (4*32+30))
  , (Feat_HYPERVISOR,  (4*32+31))
  , (Feat_XSTORE,  (5*32+ 2))
  , (Feat_XSTORE_EN,  (5*32+ 3))
  , (Feat_XCRYPT,  (5*32+ 6))
  , (Feat_XCRYPT_EN,  (5*32+ 7))
  , (Feat_ACE2,  (5*32+ 8))
  , (Feat_ACE2_EN,  (5*32+ 9))
  , (Feat_PHE,    (5*32+ 10))
  , (Feat_PHE_EN,  (5*32+ 11))
  , (Feat_PMM,    (5*32+ 12))
  , (Feat_PMM_EN,  (5*32+ 13))
  , (Feat_LAHF_LM,     (6*32+ 0))
  , (Feat_CMP_LEGACY,  (6*32+ 1))
  , (Feat_SVM,         (6*32+ 2))
  , (Feat_EXTAPIC,     (6*32+ 3))
  , (Feat_CR8_LEGACY,  (6*32+ 4))
  , (Feat_ABM,         (6*32+ 5))
  , (Feat_SSE4A,       (6*32+ 6))
  , (Feat_MISALIGNSSE, (6*32+ 7))
  , (Feat_3DNOWPREFETCH, (6*32+ 8))
  , (Feat_OSVW,        (6*32+ 9))
  , (Feat_IBS,         (6*32+10))
  , (Feat_XOP,         (6*32+11))
  , (Feat_SKINIT,      (6*32+12))
  , (Feat_WDT,         (6*32+13))
  , (Feat_LWP,         (6*32+15))
  , (Feat_FMA4,        (6*32+16))
  , (Feat_NODEID_MSR,  (6*32+19))
  , (Feat_TBM,         (6*32+21))
  , (Feat_TOPOEXT,     (6*32+22))
  , (Feat_FSGSBASE,  (7*32+ 0))
  , (Feat_BMI1,  (7*32+ 3))
  , (Feat_HLE,   (7*32+ 4))
  , (Feat_AVX2,  (7*32+ 5))
  , (Feat_SMEP,  (7*32+ 7))
  , (Feat_BMI2,  (7*32+ 8))
  , (Feat_ERMS,  (7*32+ 9))
  , (Feat_INVPCID,  (7*32+10))
  , (Feat_RTM,   (7*32+11))
  ]

