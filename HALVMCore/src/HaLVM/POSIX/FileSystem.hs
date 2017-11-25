module HaLVM.POSIX.FileSystem()
 where

import System.Posix.Types

syscall_getdents {- BROKEN -} :: CUint -> Ptr CUInt -> CUint -> IO CInt
syscall_getdents = undefined

foreign export ccall syscall_getdents ::
  CUint -> Ptr CUInt -> CUint -> IO CInt

-- -----------------------------------------------------------------------------

syscall_openat4 {- BROKEN -} :: CInt -> Ptr CChar -> CInt -> CMode -> IO CInt
syscall_openat4 = undefined

foreign export ccall syscall_openat4 ::
  CInt -> Ptr CChar -> CInt -> CMode -> IO CInt

