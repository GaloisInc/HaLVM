module HaLVM.POSIX.Unsupported() where

import Data.Bits((.&.))
import Foreign.C.Error(eACCES,eCHILD,eFAULT,eINVAL,eNOENT,eNOMEM,eNOSYS,ePERM)
import Foreign.C.Types(CChar,CInt(..),CSize(..),CULong(..),CLong(..))
import Foreign.Ptr(Ptr)
import HaLVM.POSIX.Errno(errnoReturn)

type AcctType = Ptr CChar -> IO CInt
halvm_syscall_acct :: AcctType
foreign export ccall halvm_syscall_acct :: AcctType
halvm_syscall_acct _ = errnoReturn eNOSYS

type AdjTimeX = Ptr CChar -> IO CInt -- FIXME/BROKEN?
halvm_syscall_adjtimex :: AdjTimeX
foreign export ccall halvm_syscall_adjtimex :: AdjTimeX
halvm_syscall_adjtimex _ = return 0 -- TIME_OK

type ArchPrCtl = CInt -> CULong -> IO CInt
halvm_syscall_arch_prctl :: ArchPrCtl
foreign export ccall halvm_syscall_arch_prctl :: ArchPrCtl
halvm_syscall_arch_prctl _ _ = errnoReturn eINVAL

type CapGetType = Ptr () -> Ptr () -> IO CInt
halvm_syscall_capget :: CapGetType
foreign export ccall halvm_syscall_capget :: CapGetType
halvm_syscall_capget _ _ = errnoReturn eINVAL

type CapSetType = Ptr () -> Ptr () -> IO CInt
halvm_syscall_capset :: CapSetType
foreign export ccall halvm_syscall_capset :: CapGetType
halvm_syscall_capset _ _ = errnoReturn eINVAL

type ChrootType = Ptr CChar -> IO CInt
halvm_syscall_chroot :: ChrootType
foreign export ccall halvm_syscall_chroot :: ChrootType
halvm_syscall_chroot _ = errnoReturn ePERM

type ClockAdjTime = CInt -> Ptr CChar -> IO CInt -- FIXME/BROKEN?
halvm_syscall_clock_adjtime :: ClockAdjTime
foreign export ccall halvm_syscall_clock_adjtime :: ClockAdjTime
halvm_syscall_clock_adjtime _ _ = return 0 -- TIME_OK

type DelModuleType = Ptr CChar -> CInt -> IO CInt
halvm_syscall_delete_module :: DelModuleType
foreign export ccall halvm_syscall_delete_module :: DelModuleType
halvm_syscall_delete_module _ _ = errnoReturn ePERM

type ExecveType = Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt
halvm_syscall_execve :: ExecveType
foreign export ccall halvm_syscall_execve :: ExecveType
halvm_syscall_execve _ _ _ = errnoReturn eNOMEM

type GetGroupsType = CSize -> Ptr CInt -> IO CInt
halvm_syscall_getgroups :: GetGroupsType
foreign export ccall halvm_syscall_getgroups :: GetGroupsType
halvm_syscall_getgroups _ _ = errnoReturn eFAULT

type InitModuleType = Ptr CChar -> CULong -> Ptr CChar -> IO CInt
halvm_syscall_init_module :: InitModuleType
foreign export ccall halvm_syscall_init_module :: InitModuleType
halvm_syscall_init_module _ _ _ = errnoReturn ePERM

type IoplType = CInt -> IO CInt
halvm_syscall_iopl :: IoplType
foreign export ccall halvm_syscall_iopl :: IoplType
halvm_syscall_iopl _ = errnoReturn ePERM

type MknodType = Ptr CChar -> CInt -> CInt -> IO CInt
halvm_syscall_mknod :: MknodType
foreign export ccall halvm_syscall_mknod :: MknodType
halvm_syscall_mknod _ _ _ = errnoReturn eNOMEM

type MsgCtlType = CInt -> CInt -> Ptr CChar -> IO CInt
halvm_syscall_msgctl :: MsgCtlType
foreign export ccall halvm_syscall_msgctl :: MsgCtlType
halvm_syscall_msgctl _ _ _ = errnoReturn eINVAL

type MsgRcvType = CInt -> Ptr CChar -> CSize -> CLong -> IO CInt
halvm_syscall_msgrcv :: MsgRcvType
foreign export ccall halvm_syscall_msgrcv :: MsgRcvType
halvm_syscall_msgrcv _ _ _ _ = errnoReturn eINVAL

type MsgSndType = CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
halvm_syscall_msgsnd :: MsgSndType
foreign export ccall halvm_syscall_msgsnd :: MsgSndType
halvm_syscall_msgsnd _ _ _ _ = errnoReturn eINVAL

type MsgGetType = CInt -> CInt -> IO CInt
halvm_syscall_msgget :: MsgGetType
foreign export ccall halvm_syscall_msgget :: MsgGetType
halvm_syscall_msgget _ flags | (flags .&. 512) == 0 = errnoReturn eNOENT
                       | otherwise            = errnoReturn eNOMEM

type PersonalityType = CULong -> IO CInt
halvm_syscall_personality :: PersonalityType
foreign export ccall halvm_syscall_personality :: PersonalityType
halvm_syscall_personality _ = errnoReturn eINVAL

type PivotType = Ptr CChar -> Ptr CChar -> IO CInt
halvm_syscall_pivot_root :: PivotType
foreign export ccall halvm_syscall_pivot_root :: PivotType
halvm_syscall_pivot_root _ _ = errnoReturn ePERM

type PrctlType = CInt->CULong->CULong->CULong->CULong -> IO CInt
halvm_syscall_prctl :: PrctlType
foreign export ccall halvm_syscall_prctl :: PrctlType
halvm_syscall_prctl _ _ _ _ _ = errnoReturn eINVAL

type ProcVMReadType =
  CInt->Ptr CChar->CULong->Ptr CChar->CULong->CULong->IO CInt
halvm_syscall_process_vm_readv :: ProcVMReadType
foreign export ccall halvm_syscall_process_vm_readv :: ProcVMReadType
halvm_syscall_process_vm_readv _ _ _ _ _ _ = errnoReturn ePERM

type ProcVMWriteType =
  CInt->Ptr CChar->CULong->Ptr CChar->CULong->CULong->IO CInt
halvm_syscall_process_vm_writev :: ProcVMWriteType
foreign export ccall halvm_syscall_process_vm_writev :: ProcVMWriteType
halvm_syscall_process_vm_writev _ _ _ _ _ _ = errnoReturn ePERM

type PTraceType = CInt -> CInt -> Ptr CChar -> Ptr CChar -> IO CInt
halvm_syscall_ptrace :: PTraceType
foreign export ccall halvm_syscall_ptrace :: PTraceType
halvm_syscall_ptrace _ _ _ _ = errnoReturn eINVAL

type RemapType = Ptr CChar -> CSize -> CInt -> CSize -> CInt -> IO CInt
halvm_syscall_remap_file_pages :: RemapType
foreign export ccall halvm_syscall_remap_file_pages :: RemapType
halvm_syscall_remap_file_pages _ _ _ _ _ = errnoReturn eINVAL

type SetFSGIDType = CInt -> IO CInt
halvm_syscall_setfsgid :: SetFSGIDType
foreign export ccall halvm_syscall_setfsgid :: SetFSGIDType
halvm_syscall_setfsgid _ = return 17 -- don't really care

type SetFSUIDType = CInt -> IO CInt
halvm_syscall_setfsuid :: SetFSUIDType
foreign export ccall halvm_syscall_setfsuid :: SetFSUIDType
halvm_syscall_setfsuid _ = return 22 -- nope, still don't really care

type SetGroupsType = CSize -> Ptr CChar -> IO CInt
halvm_syscall_setgroups :: SetGroupsType
foreign export ccall halvm_syscall_setgroups :: SetGroupsType
halvm_syscall_setgroups _ _ = errnoReturn ePERM

type SetNSType = CInt -> CInt -> IO CInt
halvm_syscall_setns :: SetNSType
foreign export ccall halvm_syscall_setns :: SetNSType
halvm_syscall_setns _ _ = errnoReturn eINVAL

type SetPGID = CInt -> CInt -> IO CInt
halvm_syscall_setpgid :: SetPGID
foreign export ccall halvm_syscall_setpgid :: SetPGID
halvm_syscall_setpgid _ _ = errnoReturn ePERM

type SetSID = IO CInt
halvm_syscall_setsid :: SetSID
foreign export ccall halvm_syscall_setsid :: SetSID
halvm_syscall_setsid = errnoReturn ePERM

type SHMat = CInt -> Ptr CChar -> CInt -> IO CInt
halvm_syscall_shmat :: SHMat
foreign export ccall halvm_syscall_shmat :: SHMat
halvm_syscall_shmat _ _ _ = errnoReturn eINVAL

type SHMctl = CInt -> CInt -> Ptr CChar -> IO CInt
halvm_syscall_shmctl :: SHMctl
foreign export ccall halvm_syscall_shmctl :: SHMctl
halvm_syscall_shmctl _ _ _ = errnoReturn eINVAL

type SHMdt = Ptr CChar -> IO CInt
halvm_syscall_shmdt :: SHMdt
foreign export ccall halvm_syscall_shmdt :: SHMdt
halvm_syscall_shmdt _ = errnoReturn eINVAL

type SHMget = CInt -> CSize -> CInt -> IO CInt
halvm_syscall_shmget :: SHMget
foreign export ccall halvm_syscall_shmget :: SHMget
halvm_syscall_shmget _ _ _ = errnoReturn eACCES

type SwapOff = Ptr CChar -> IO CInt
halvm_syscall_swapoff :: SwapOff
foreign export ccall halvm_syscall_swapoff :: SwapOff
halvm_syscall_swapoff _ = errnoReturn eINVAL

type SwapOn = Ptr CChar -> IO CInt
halvm_syscall_swapon :: SwapOn
foreign export ccall halvm_syscall_swapon :: SwapOn
halvm_syscall_swapon _ = errnoReturn ePERM

type Unshare = CInt -> IO CInt
halvm_syscall_unshare :: Unshare
foreign export ccall halvm_syscall_unshare :: Unshare
halvm_syscall_unshare _ = errnoReturn ePERM

type VHangup = IO CInt
halvm_syscall_vhanghup :: VHangup
foreign export ccall halvm_syscall_vhanghup :: VHangup
halvm_syscall_vhanghup = errnoReturn ePERM

type Wait4 = CInt -> Ptr CInt -> CInt -> Ptr CChar -> IO CInt
halvm_syscall_wait4 :: Wait4
foreign export ccall halvm_syscall_wait4 :: Wait4
halvm_syscall_wait4 _ _ _ _ = errnoReturn eCHILD

type Waitid = CInt -> CInt -> Ptr () -> CInt -> IO CInt
halvm_syscall_waitid :: Waitid
foreign export ccall halvm_syscall_waitid :: Waitid
halvm_syscall_waitid _ _ _ _ = errnoReturn eCHILD

