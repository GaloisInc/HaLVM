module HaLVM.POSIX.FileSystem()
 where

import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types

syscall_getdents {- BROKEN -} :: CUInt -> Ptr CUInt -> CUInt -> IO CInt
syscall_getdents = undefined

foreign export ccall syscall_getdents ::
  CUInt -> Ptr CUInt -> CUInt -> IO CInt

-- -----------------------------------------------------------------------------

syscall_openat4 {- BROKEN -} :: CInt -> Ptr CChar -> CInt -> CMode -> IO CInt
syscall_openat4 = undefined

foreign export ccall syscall_openat4 ::
  CInt -> Ptr CChar -> CInt -> CMode -> IO CInt

