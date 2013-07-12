-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Structures.VCPUContext(
         FPUContext(..)
       , ProcessorFlag(..)
       , ProcessorUserRegs(..)
       , TrapInfo(..)
       , VMAssistFlag(..)
       , ProcessorContext(..)
       )
 where

import Control.Monad
import Data.Bits
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

#include <stdint.h>
#define __XEN_TOOLS__
#include <xen/domctl.h>
#include "ghcplatform.h"

-- |The state of the FPU, generated from something like FXSAVE
data FPUContext = FPUContext {
    fpuFCW        :: Word16
  , fpuFSW        :: Word16
  , fpuFTW        :: Word8
  , fpuFOP        :: Word16
#ifdef x86_64_TARGET_ARCH
  , fpuIP         :: Word64
  , fpuDP         :: Word64
#else
  , fpuIP         :: Word32
  , fpuCS         :: Word16
  , fpuDP         :: Word32
  , fpuDS         :: Word16
#endif
  , fpuMXCSR      :: Word32
  , fpuMXCSRMask  :: Word32
  , fpuMMRegs     :: [[Word8]] -- ^Each MM variables, 80 bits each
  , fpuXMMRegs    :: [[Word8]] -- ^Each XMM variable, 128 bits each
  }
 deriving (Eq, Show, Generic)

data ProcessorFlag = ProcessorI387
                   | ProcessorInKernel
                   | ProcessorFailsafeDisablesEvents
                   | ProcessorSyscallDisablesEvents
                   | ProcessorOnline
  deriving (Eq, Show, Generic)

#ifdef x86_64_TARGET_ARCH
data ProcessorUserRegs = ProcessorUserRegs {
    urR15             :: Word64
  , urR14             :: Word64
  , urR13             :: Word64
  , urR12             :: Word64
  , urR11             :: Word64
  , urR10             :: Word64
  , urR9              :: Word64
  , urR8              :: Word64
  , urRBP             :: Word64
  , urRAX             :: Word64
  , urRBX             :: Word64
  , urRCX             :: Word64
  , urRDX             :: Word64
  , urRSI             :: Word64
  , urRDI             :: Word64
  , urErrorCode       :: Word32
  , urEntryVector     :: Word32
  , urSavedUpcallMask :: Word8
  , urFlags           :: Word64
  , urRIP             :: Word64
  , urRSP             :: Word64
  , urCS              :: Word16
  , urDS              :: Word16
  , urES              :: Word16
  , urFS              :: Word16
  , urGS              :: Word16
  , urSS              :: Word16
  }
 deriving (Eq, Show, Generic)
#else
data ProcessorUserRegs = ProcessorUserRegs {
    urEBX             :: Word32
  , urECX             :: Word32
  , urEDX             :: Word32
  , urESI             :: Word32
  , urEDI             :: Word32
  , urEBP             :: Word32
  , urEAX             :: Word32
  , urErrorCode       :: Word16
  , urEntryVector     :: Word16
  , urEIP             :: Word32
  , urCS              :: Word16
  , urSavedUpcallMask :: Word8
  , urEFLAGS          :: Word32
  , urESP             :: Word32
  , urSS              :: Word16
  , urES              :: Word16
  , urDS              :: Word16
  , urFS              :: Word16
  , urGS              :: Word16
  }
 deriving (Eq, Show, Generic)
#endif

data TrapInfo = TrapInfo {
    tiVector    :: Word8
  , tiFlags     :: Word8
  , tiCS        :: Word16
  , tiAddress   :: Word
  }
 deriving (Eq, Show, Generic)

data VMAssistFlag = VMAssist4GBSegments
                  | VMAssist4GBSegmentsNotify
                  | VMAssistWritablePageTables
                  | VMAssistPAEExtendedCR3
 deriving (Eq, Show, Generic)

data ProcessorContext = ProcessorContext {
    rcFPU                :: FPUContext
  , rcProcFlags          :: [ProcessorFlag]
  , rcUserRegs           :: ProcessorUserRegs
  , rcTrapInfo           :: [TrapInfo]
  , rcLDTBase            :: Word
  , rcLDTEntries         :: Word
  , rcGDTFrames          :: [Word]
  , rcGDTEntries         :: Word
  , rcKernelSS           :: Word
  , rcKernelSP           :: Word
  , rcControlRegs        :: [Word]
  , rcDebugRegs          :: [Word8]
#ifdef x86_64_TARGET_ARCH
  , rcEventCallbackIP    :: Word
  , rcFailsafeCallbackIP :: Word
#else
  , rcEventCallbackCS    :: Word
  , rcEventCallbackIP    :: Word
  , rcFailsafeCallbackCS :: Word
  , rcFailsafeCallbackIP :: Word
#endif
  , rcSyscallCallbackIP  :: Word
  , rcVMAssist           :: [VMAssistFlag]
#ifdef x86_64_TARGET_ARCH
  , rcFSBase             :: Word64
  , rcGSBaseKernel       :: Word64
  , rcGSBaseUser         :: Word64
#endif
  }
 deriving (Eq, Show, Generic)


readSTAt :: Ptr a -> Int -> IO [Word8]
readSTAt p off = peekArray 10 (castPtr p `plusPtr` off)

writeSTAt :: Ptr a -> [Word8] -> Int -> IO ()
writeSTAt p v off = pokeArray (castPtr p `plusPtr` off) (take 10 v)

readXMMAt :: Ptr a -> Int -> IO [Word8]
readXMMAt p off = peekArray 16 (castPtr p `plusPtr` off)

writeXMMAt :: Ptr a -> [Word8] -> Int -> IO ()
writeXMMAt p v off = pokeArray (castPtr p `plusPtr` off) (take 16 v)

instance Storable FPUContext where
  sizeOf _         = 512
  alignment _      = 16
  peek p           = do
    fcw <- peekByteOff p 0
    fsw <- peekByteOff p 2
    ftw <- peekByteOff p 4
    fop <- peekByteOff p 6
#ifdef x86_64_TARGET_ARCH
    fip <- peekByteOff p 8
    fdp <- peekByteOff p 16
#else
    fip <- peekByteOff p 8
    fcs <- peekByteOff p 12
    fdp <- peekByteOff p 16
    fds <- peekByteOff p 20
#endif
    fmc <- peekByteOff p 24
    fmk <- peekByteOff p 28
    mms <- mapM (readSTAt p)  [32,48,64,80,96,112,128,144]
    xms <- mapM (readXMMAt p) [160,176,192,206,224,240,256,272]
    return (FPUContext fcw fsw ftw fop
#ifdef x86_64_TARGET_ARCH
                       fip fdp
#else
                       fip fcs fdp fds
#endif
                       fmc fmk mms xms)
  poke p v         = do
    pokeByteOff p 0 (fpuFCW v)
    pokeByteOff p 2 (fpuFSW v)
    pokeByteOff p 4 (fpuFTW v)
    pokeByteOff p 6 (fpuFOP v)
    pokeByteOff p 8 (fpuIP v)
#ifndef x86_64_TARGET_ARCH
    pokeByteOff p 12 (fpuCS v)
#endif
    pokeByteOff p 16 (fpuDP v)
#ifndef x86_64_TARGET_ARCH
    pokeByteOff p 20 (fpuDS v)
#endif
    pokeByteOff p 24 (fpuMXCSR v)
    pokeByteOff p 28 (fpuMXCSRMask v)
    zipWithM_ (writeSTAt p) (fpuMMRegs v) [32,48,64,80,96,112,128,144]
    zipWithM_ (writeXMMAt p) (fpuXMMRegs v) [160,176,192,206,224,240,256,272]

instance Storable [ProcessorFlag] where
  sizeOf _    = sizeOf (undefined :: Word)
  alignment _ = 1
  peek p      = do
    val <- peek (castPtr p) :: IO Word
    return $ mAddFlag val (#const VGCF_i387_valid) ProcessorI387
           $ mAddFlag val (#const VGCF_in_kernel)  ProcessorInKernel
           $ mAddFlag val (#const VGCF_failsafe_disables_events)
                      ProcessorFailsafeDisablesEvents
           $ mAddFlag val (#const VGCF_syscall_disables_events)
                      ProcessorSyscallDisablesEvents
           $ mAddFlag val (#const VGCF_online) ProcessorOnline []
   where
    mAddFlag x c v r | x .&. c /= 0 = v : r
                     | otherwise    = r
  poke p v     = do
    let val = mAddFlag (#const VGCF_i387_valid) ProcessorI387
            $ mAddFlag (#const VGCF_in_kernel)  ProcessorInKernel
            $ mAddFlag (#const VGCF_failsafe_disables_events)
                       ProcessorFailsafeDisablesEvents
            $ mAddFlag (#const VGCF_syscall_disables_events)
                       ProcessorSyscallDisablesEvents
            $ mAddFlag (#const VGCF_online) ProcessorOnline 0
    poke (castPtr p) (val :: Word)
   where mAddFlag c f r | f `elem` v = c .|. r
                        | otherwise  = r

#ifdef x86_64_TARGET_ARCH
instance Storable ProcessorUserRegs where
  sizeOf _           = (#size cpu_user_regs_t)
  alignment _        = 16
  peek p             = do
    r5 <- (#peek cpu_user_regs_t,r15) p
    r4 <- (#peek cpu_user_regs_t,r14) p
    r3 <- (#peek cpu_user_regs_t,r13) p
    r2 <- (#peek cpu_user_regs_t,r12) p
    r1 <- (#peek cpu_user_regs_t,r11) p
    r0 <- (#peek cpu_user_regs_t,r10) p
    r9 <- (#peek cpu_user_regs_t,r9) p
    r8 <- (#peek cpu_user_regs_t,r8) p
    bp <- (#peek cpu_user_regs_t,rbp) p
    ax <- (#peek cpu_user_regs_t,rax) p
    bx <- (#peek cpu_user_regs_t,rbx) p
    cx <- (#peek cpu_user_regs_t,rcx) p
    dx <- (#peek cpu_user_regs_t,rdx) p
    si <- (#peek cpu_user_regs_t,rsi) p
    di <- (#peek cpu_user_regs_t,rdi) p
    ec <- (#peek cpu_user_regs_t,error_code) p
    ev <- (#peek cpu_user_regs_t,entry_vector) p
    um <- (#peek cpu_user_regs_t,saved_upcall_mask) p
    fl <- (#peek cpu_user_regs_t,rflags) p
    ip <- (#peek cpu_user_regs_t,rip) p
    sp <- (#peek cpu_user_regs_t,rsp) p
    cs <- (#peek cpu_user_regs_t,cs) p
    ds <- (#peek cpu_user_regs_t,ds) p
    es <- (#peek cpu_user_regs_t,es) p
    fs <- (#peek cpu_user_regs_t,fs) p
    gs <- (#peek cpu_user_regs_t,gs) p
    ss <- (#peek cpu_user_regs_t,ss) p
    return (ProcessorUserRegs r5 r4 r3 r2 r1 r0 r9 r8 bp ax bx cx dx si di
                              ec ev um fl ip sp cs ds es fs gs ss)
  poke p v = do
   (#poke cpu_user_regs_t,r15)               p (urR15 v)
   (#poke cpu_user_regs_t,r14)               p (urR15 v)
   (#poke cpu_user_regs_t,r13)               p (urR15 v)
   (#poke cpu_user_regs_t,r12)               p (urR15 v)
   (#poke cpu_user_regs_t,rbp)               p (urRBP v)
   (#poke cpu_user_regs_t,rbx)               p (urRBX v)
   (#poke cpu_user_regs_t,r11)               p (urR15 v)
   (#poke cpu_user_regs_t,r10)               p (urR15 v)
   (#poke cpu_user_regs_t,r9)                p (urR15 v)
   (#poke cpu_user_regs_t,r8)                p (urR15 v)
   (#poke cpu_user_regs_t,rax)               p (urRAX v)
   (#poke cpu_user_regs_t,rcx)               p (urRCX v)
   (#poke cpu_user_regs_t,rdx)               p (urRDX v)
   (#poke cpu_user_regs_t,rsi)               p (urRSI v)
   (#poke cpu_user_regs_t,rdi)               p (urRDI v)
   (#poke cpu_user_regs_t,error_code)        p (urErrorCode v)
   (#poke cpu_user_regs_t,entry_vector)      p (urEntryVector v)
   (#poke cpu_user_regs_t,rip)               p (urRIP v)
   (#poke cpu_user_regs_t,cs)                p (urCS v)
   (#poke cpu_user_regs_t,saved_upcall_mask) p (urSavedUpcallMask v)
   (#poke cpu_user_regs_t,rflags)            p (urFlags v)
   (#poke cpu_user_regs_t,rsp)               p (urRSP v)
   (#poke cpu_user_regs_t,ss)                p (urSS v)
   (#poke cpu_user_regs_t,es)                p (urES v)
   (#poke cpu_user_regs_t,ds)                p (urDS v)
   (#poke cpu_user_regs_t,fs)                p (urFS v)
   (#poke cpu_user_regs_t,gs)                p (urGS v)
#else
instance Storable ProcessorUserRegs where
  sizeOf _           = (#size cpu_user_regs_t)
  alignment _        = 16
  peek p             = do
    bx <- (#peek cpu_user_regs_t,ebx) p
    cx <- (#peek cpu_user_regs_t,ecx) p
    dx <- (#peek cpu_user_regs_t,edx) p
    si <- (#peek cpu_user_regs_t,esi) p
    di <- (#peek cpu_user_regs_t,edi) p
    bp <- (#peek cpu_user_regs_t,ebp) p
    ax <- (#peek cpu_user_regs_t,eax) p
    ec <- (#peek cpu_user_regs_t,error_code) p
    ev <- (#peek cpu_user_regs_t,entry_vector) p
    ip <- (#peek cpu_user_regs_t,eip) p
    cs <- (#peek cpu_user_regs_t,cs) p
    um <- (#peek cpu_user_regs_t,saved_upcall_mask) p
    fl <- (#peek cpu_user_regs_t,eflags) p
    sp <- (#peek cpu_user_regs_t,esp) p
    ss <- (#peek cpu_user_regs_t,ss) p
    es <- (#peek cpu_user_regs_t,es) p
    ds <- (#peek cpu_user_regs_t,ds) p
    fs <- (#peek cpu_user_regs_t,fs) p
    gs <- (#peek cpu_user_regs_t,gs) p
    return (ProcessorUserRegs bx cx dx si di bp ax ec ev ip cs
                              um fl sp ss es ds fs gs)
  poke p v           = do
   (#poke cpu_user_regs_t,ebx)               p (urEBX v)
   (#poke cpu_user_regs_t,ecx)               p (urECX v)
   (#poke cpu_user_regs_t,edx)               p (urEDX v)
   (#poke cpu_user_regs_t,esi)               p (urESI v)
   (#poke cpu_user_regs_t,edi)               p (urEDI v)
   (#poke cpu_user_regs_t,ebp)               p (urEBP v)
   (#poke cpu_user_regs_t,eax)               p (urEAX v)
   (#poke cpu_user_regs_t,error_code)        p (urErrorCode v)
   (#poke cpu_user_regs_t,entry_vector)      p (urEntryVector v)
   (#poke cpu_user_regs_t,eip)               p (urEIP v)
   (#poke cpu_user_regs_t,cs)                p (urCS v)
   (#poke cpu_user_regs_t,saved_upcall_mask) p (urSavedUpcallMask v)
   (#poke cpu_user_regs_t,eflags)            p (urEFLAGS v)
   (#poke cpu_user_regs_t,esp)               p (urESP v)
   (#poke cpu_user_regs_t,ss)                p (urSS v)
   (#poke cpu_user_regs_t,es)                p (urES v)
   (#poke cpu_user_regs_t,ds)                p (urDS v)
   (#poke cpu_user_regs_t,fs)                p (urFS v)
   (#poke cpu_user_regs_t,gs)                p (urGS v)
#endif

instance Storable TrapInfo where
  sizeOf _      = (#size trap_info_t)
  alignment _   = 1
  peek p        = do
    v <- (#peek trap_info_t,vector)  p
    f <- (#peek trap_info_t,flags)   p
    c <- (#peek trap_info_t,cs)      p
    a <- (#peek trap_info_t,address) p
    return (TrapInfo v f c a)
  poke p v      = do
    (#poke trap_info_t,vector)  p (tiVector v)
    (#poke trap_info_t,flags)   p (tiFlags v)
    (#poke trap_info_t,cs)      p (tiCS v)
    (#poke trap_info_t,address) p (tiAddress v)

instance Storable [VMAssistFlag] where
  sizeOf    _   = sizeOf (undefined :: Word)
  alignment _   = 1
  peek      p   = do
    x <- peek (castPtr p :: Ptr Word)
    return $ mAddFlag x (#const VMASST_TYPE_4gb_segments) VMAssist4GBSegments
           $ mAddFlag x (#const VMASST_TYPE_4gb_segments_notify)
                        VMAssist4GBSegmentsNotify
           $ mAddFlag x (#const VMASST_TYPE_writable_pagetables)
                        VMAssistWritablePageTables
           $ mAddFlag x (#const VMASST_TYPE_pae_extended_cr3)
                        VMAssistPAEExtendedCR3
           $ []
   where mAddFlag x c v r | testBit x c = v : r
                          | otherwise   = r
  poke      p x = do
    let val = mAddFlag (#const VMASST_TYPE_4gb_segments) VMAssist4GBSegments
            $ mAddFlag (#const VMASST_TYPE_4gb_segments_notify)
                       VMAssist4GBSegmentsNotify
            $ mAddFlag (#const VMASST_TYPE_writable_pagetables)
                       VMAssistWritablePageTables
            $ mAddFlag (#const VMASST_TYPE_pae_extended_cr3)
                       VMAssistPAEExtendedCR3
                       0
    poke (castPtr p :: Ptr Word) (val :: Word)
   where mAddFlag c v r | v `elem` x = setBit r c
                        | otherwise  = r

instance Storable ProcessorContext where
  sizeOf _    = (#size vcpu_guest_context_t)
  alignment _ = 16
  peek p      = do
    fpu <- (#peek vcpu_guest_context_t,fpu_ctxt)  p
    fla <- (#peek vcpu_guest_context_t,flags)     p
    usr <- (#peek vcpu_guest_context_t,user_regs) p
    tct <- peekArray 256 (p `plusPtr` (#offset vcpu_guest_context_t,trap_ctxt))
    ldb <- (#peek vcpu_guest_context_t,ldt_base)  p
    lde <- (#peek vcpu_guest_context_t,ldt_ents)  p
    gdb <- peekArray 16 (p `plusPtr` (#offset vcpu_guest_context_t,gdt_frames))
    gde <- (#peek vcpu_guest_context_t,gdt_ents)  p
    kss <- (#peek vcpu_guest_context_t,kernel_ss) p
    ksp <- (#peek vcpu_guest_context_t,kernel_sp) p
    cts <- peekArray 8 (p `plusPtr` (#offset vcpu_guest_context_t,ctrlreg))
    dbs <- peekArray 8 (p `plusPtr` (#offset vcpu_guest_context_t,debugreg))
    eci <- (#peek vcpu_guest_context_t,event_callback_eip) p
    fci <- (#peek vcpu_guest_context_t,failsafe_callback_eip) p
#ifndef x86_64_TARGET_ARCH
    ecs <- (#peek vcpu_guest_context_t,event_callback_cs) p
    fcs <- (#peek vcpu_guest_context_t,failsafe_callback_cs) p
#endif
    sci <- (#peek vcpu_guest_context_t,syscall_callback_eip) p
    vma <- (#peek vcpu_guest_context_t,vm_assist) p
#ifdef x86_64_TARGET_ARCH
    fsb <- (#peek vcpu_guest_context_t,fs_base) p
    gsk <- (#peek vcpu_guest_context_t,gs_base_kernel) p
    gsu <- (#peek vcpu_guest_context_t,gs_base_user) p
#endif
    return (ProcessorContext fpu fla usr tct ldb lde gdb gde kss ksp cts dbs
#ifdef x86_64_TARGET_ARCH
                             eci fci sci vma fsb gsk gsu
#else
                             ecs eci fcs fci sci vma
#endif
                             )
  poke p v    = do
    (#poke vcpu_guest_context_t,fpu_ctxt)    p (rcFPU v)
    (#poke vcpu_guest_context_t,flags)       p (rcProcFlags v)
    (#poke vcpu_guest_context_t,user_regs)   p (rcUserRegs v)
    pokeArray (p `plusPtr` (#offset vcpu_guest_context_t,trap_ctxt))
              (take 256 (rcTrapInfo v))
    (#poke vcpu_guest_context_t,ldt_base)    p (rcLDTBase v)
    (#poke vcpu_guest_context_t,ldt_ents)    p (rcLDTEntries v)
    pokeArray (p `plusPtr` (#offset vcpu_guest_context_t,gdt_frames))
              (take 16 (rcGDTFrames v))
    (#poke vcpu_guest_context_t,gdt_ents)    p (rcGDTEntries v)
    (#poke vcpu_guest_context_t,kernel_ss)   p (rcKernelSS v)
    (#poke vcpu_guest_context_t,kernel_sp)   p (rcKernelSP v)
    pokeArray (p `plusPtr` (#offset vcpu_guest_context_t,ctrlreg))
              (take 8 (rcControlRegs v))
    pokeArray (p `plusPtr` (#offset vcpu_guest_context_t,debugreg))
              (take 8 (rcDebugRegs v))
    (#poke vcpu_guest_context_t,event_callback_eip) p (rcEventCallbackIP v)
    (#poke vcpu_guest_context_t,failsafe_callback_eip)p (rcFailsafeCallbackIP v)
    (#poke vcpu_guest_context_t,syscall_callback_eip) p (rcSyscallCallbackIP v)
    (#poke vcpu_guest_context_t,vm_assist) p (rcVMAssist v)
#ifdef x86_64_TARGET_ARCH
    (#poke vcpu_guest_context_t,fs_base) p (rcFSBase v)
    (#poke vcpu_guest_context_t,gs_base_kernel) p (rcGSBaseKernel v)
    (#poke vcpu_guest_context_t,gs_base_user) p (rcGSBaseUser v)
#else
    (#poke vcpu_guest_context_t,event_callback_cs) p (rcEventCallbackCS v)
    (#poke vcpu_guest_context_t,failsafe_callback_cs) p (rcFailsafeCallbackCS v)
#endif
