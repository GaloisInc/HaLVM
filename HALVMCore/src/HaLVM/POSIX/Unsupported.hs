module HaLVM.POSIX.Unsupported() where

import Data.Bits((.&.))
import Foreign.C.Error(eACCES,eCHILD,eFAULT,eINVAL,eNOENT,eNOMEM,eNOSYS,ePERM)
import Foreign.C.Types(CChar,CInt(..),CSize(..),CULong(..),CLong(..))
import Foreign.Ptr(Ptr)
import HaLVM.POSIX.Errno(errnoReturn)

type AcctType = Ptr CChar -> IO CInt
syscall_acct :: AcctType
foreign export ccall syscall_acct :: AcctType
syscall_acct _ = errnoReturn eNOSYS

type AdjTimeX = Ptr CChar -> IO CInt
syscall_adjtimex :: AdjTimeX
foreign export ccall syscall_adjtimex :: AdjTimeX
syscall_adjtimex _ = return 0 -- TIME_OK

type ArchPrCtl = CInt -> CULong -> IO CInt
syscall_arch_prctl :: ArchPrCtl
foreign export ccall syscall_arch_prctl :: ArchPrCtl
syscall_arch_prctl _ _ = errnoReturn eINVAL

type CapGetType = CInt -> CInt -> IO CInt
syscall_capget :: CapGetType
foreign export ccall syscall_capget :: CapGetType
syscall_capget _ _ = errnoReturn eINVAL

type CapSetType = CInt -> CInt -> IO CInt
syscall_capset :: CapSetType
foreign export ccall syscall_capset :: CapGetType
syscall_capset _ _ = errnoReturn eINVAL

type ChrootType = Ptr CChar -> IO CInt
syscall_chroot :: ChrootType
foreign export ccall syscall_chroot :: ChrootType
syscall_chroot _ = errnoReturn ePERM

type DelModuleType = Ptr CChar -> CInt -> IO CInt
syscall_delete_module :: DelModuleType
foreign export ccall syscall_delete_module :: DelModuleType
syscall_delete_module _ _ = errnoReturn ePERM

type ExecveType = Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt
syscall_execve :: ExecveType
foreign export ccall syscall_execve :: ExecveType
syscall_execve _ _ _ = errnoReturn eNOMEM

type GetGroupsType = CSize -> Ptr CInt -> IO CInt
syscall_getgroups :: GetGroupsType
foreign export ccall syscall_getgroups :: GetGroupsType
syscall_getgroups _ _ = errnoReturn eFAULT

type InitModuleType = Ptr CChar -> CULong -> Ptr CChar -> IO CInt
syscall_init_module :: InitModuleType
foreign export ccall syscall_init_module :: InitModuleType
syscall_init_module _ _ _ = errnoReturn ePERM

type IoplType = CInt -> IO CInt
syscall_iopl :: IoplType
foreign export ccall syscall_iopl :: IoplType
syscall_iopl _ = errnoReturn ePERM

type MknodType = Ptr CChar -> CInt -> CInt -> IO CInt
syscall_mknod :: MknodType
foreign export ccall syscall_mknod :: MknodType
syscall_mknod _ _ _ = errnoReturn eNOMEM

type MsgCtlType = CInt -> CInt -> Ptr CChar -> IO CInt
syscall_msgctl :: MsgCtlType
foreign export ccall syscall_msgctl :: MsgCtlType
syscall_msgctl _ _ _ = errnoReturn eINVAL

type MsgRcvType = CInt -> Ptr CChar -> CSize -> CLong -> IO CInt
syscall_msgrcv :: MsgRcvType
foreign export ccall syscall_msgrcv :: MsgRcvType
syscall_msgrcv _ _ _ _ = errnoReturn eINVAL

type MsgSndType = CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
syscall_msgsnd :: MsgSndType
foreign export ccall syscall_msgsnd :: MsgSndType
syscall_msgsnd _ _ _ _ = errnoReturn eINVAL

type MsgGetType = CInt -> CInt -> IO CInt
syscall_msgget :: MsgGetType
foreign export ccall syscall_msgget :: MsgGetType
syscall_msgget _ flags | (flags .&. 512) == 0 = errnoReturn eNOENT
                       | otherwise            = errnoReturn eNOMEM

type PersonalityType = CULong -> IO CInt
syscall_personality :: PersonalityType
foreign export ccall syscall_personality :: PersonalityType
syscall_personality _ = errnoReturn eINVAL

type PivotType = Ptr CChar -> Ptr CChar -> IO CInt
syscall_pivot_root :: PivotType
foreign export ccall syscall_pivot_root :: PivotType
syscall_pivot_root _ _ = errnoReturn ePERM

type PrctlType = CInt->CInt->CInt->CInt->CInt -> IO CInt
syscall_prctl :: PrctlType
foreign export ccall syscall_prctl :: PrctlType
syscall_prctl _ _ _ _ _ = errnoReturn eINVAL

type ProcVMReadType =
  CInt->Ptr CChar->CULong->Ptr CChar->CULong->CULong->IO CInt
syscall_process_vm_readv :: ProcVMReadType
foreign export ccall syscall_process_vm_readv :: ProcVMReadType
syscall_process_vm_readv _ _ _ _ _ _ = errnoReturn ePERM

type ProcVMWriteType =
  CInt->Ptr CChar->CULong->Ptr CChar->CULong->CULong->IO CInt
syscall_process_vm_writev :: ProcVMWriteType
foreign export ccall syscall_process_vm_writev :: ProcVMWriteType
syscall_process_vm_writev _ _ _ _ _ _ = errnoReturn ePERM

type PTraceType = CInt -> CInt -> Ptr CChar -> Ptr CChar -> IO CInt
syscall_ptrace :: PTraceType
foreign export ccall syscall_ptrace :: PTraceType
syscall_ptrace _ _ _ _ = errnoReturn eINVAL

type RemapType = Ptr CChar -> CSize -> CInt -> CSize -> CInt -> IO CInt
syscall_remap_file_pages :: RemapType
foreign export ccall syscall_remap_file_pages :: RemapType
syscall_remap_file_pages _ _ _ _ _ = errnoReturn eINVAL

type SetFSGIDType = CInt -> IO CInt
syscall_setfsgid :: SetFSGIDType
foreign export ccall syscall_setfsgid :: SetFSGIDType
syscall_setfsgid _ = return 17 -- don't really care

type SetFSUIDType = CInt -> IO CInt
syscall_setfsuid :: SetFSUIDType
foreign export ccall syscall_setfsuid :: SetFSUIDType
syscall_setfsuid _ = return 22 -- nope, still don't really care

type SetGroupsType = CSize -> Ptr CChar -> IO CInt
syscall_setgroups :: SetGroupsType
foreign export ccall syscall_setgroups :: SetGroupsType
syscall_setgroups _ _ = errnoReturn ePERM

type SetNSType = CInt -> CInt -> IO CInt
syscall_setns :: SetNSType
foreign export ccall syscall_setns :: SetNSType
syscall_setns _ _ = errnoReturn eINVAL

type SetPGID = CInt -> CInt -> IO CInt
syscall_setpgid :: SetPGID
foreign export ccall syscall_setpgid :: SetPGID
syscall_setpgid _ _ = errnoReturn ePERM

type SetSID = IO CInt
syscall_setsid :: SetSID
foreign export ccall syscall_setsid :: SetSID
syscall_setsid = errnoReturn ePERM

type SHMat = CInt -> Ptr CChar -> CInt -> IO CInt
syscall_shmat :: SHMat
foreign export ccall syscall_shmat :: SHMat
syscall_shmat _ _ _ = errnoReturn eINVAL

type SHMctl = CInt -> CInt -> Ptr CChar -> IO CInt
syscall_shmctl :: SHMctl
foreign export ccall syscall_shmctl :: SHMctl
syscall_shmctl _ _ _ = errnoReturn eINVAL

type SHMdt = Ptr CChar -> IO CInt
syscall_shmdt :: SHMdt
foreign export ccall syscall_shmdt :: SHMdt
syscall_shmdt _ = errnoReturn eINVAL

type SHMget = CInt -> CSize -> CInt -> IO CInt
syscall_shmget :: SHMget
foreign export ccall syscall_shmget :: SHMget
syscall_shmget _ _ _ = errnoReturn eACCES

type SwapOff = Ptr CChar -> IO CInt
syscall_swapoff :: SwapOff
foreign export ccall syscall_swapoff :: SwapOff
syscall_swapoff _ = errnoReturn eINVAL

type SwapOn = Ptr CChar -> IO CInt
syscall_swapon :: SwapOn
foreign export ccall syscall_swapon :: SwapOn
syscall_swapon _ = errnoReturn ePERM

type Unshare = CInt -> IO CInt
syscall_unshare :: Unshare
foreign export ccall syscall_unshare :: Unshare
syscall_unshare _ = errnoReturn ePERM

type VHangup = IO CInt
syscall_vhanghup :: VHangup
foreign export ccall syscall_vhanghup :: VHangup
syscall_vhanghup = errnoReturn ePERM

type Wait4 = CInt -> Ptr CInt -> CInt -> Ptr CChar -> IO CInt
syscall_wait4 :: Wait4
foreign export ccall syscall_wait4 :: Wait4
syscall_wait4 _ _ _ _ = errnoReturn eCHILD

type Waitid = CInt -> Ptr CInt -> CInt -> Ptr CChar -> IO CInt
syscall_waitid :: Waitid
foreign export ccall syscall_waitid :: Waitid
syscall_waitid _ _ _ _ = errnoReturn eCHILD

