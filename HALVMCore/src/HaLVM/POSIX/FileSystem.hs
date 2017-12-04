module HaLVM.POSIX.FileSystem()
 where

import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types

halvm_syscall_getdents {- BROKEN -} :: CUInt -> Ptr CUInt -> CUInt -> IO CInt
halvm_syscall_getdents = undefined

foreign export ccall halvm_syscall_getdents ::
  CUInt -> Ptr CUInt -> CUInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_syscall_getdents64 {- BROKEN -} :: CUInt -> Ptr CUInt -> CUInt -> IO CInt
halvm_syscall_getdents64 = undefined

foreign export ccall halvm_syscall_getdents64 ::
  CUInt -> Ptr CUInt -> CUInt -> IO CInt

