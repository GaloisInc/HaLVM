module HaLVM.POSIX.Semaphores()
 where

import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types

-- this appears to only be used in its four argument form by musl
syscall_semctl {- BROKEN -} :: CInt -> CInt -> CInt -> Ptr () -> IO CInt
syscall_semctl = undefined

foreign export ccall syscall_semctl ::
  CInt -> CInt -> CInt -> Ptr () -> IO CInt

-- -----------------------------------------------------------------------------

syscall_semget {- BROKEN -} :: CKey -> CInt -> CInt -> IO CInt
syscall_semget = undefined

foreign export ccall syscall_semget ::
  CKey -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

syscall_semop {- BROKEN -} :: CInt -> Ptr () -> CSize -> IO CInt
syscall_semop = undefined

foreign export ccall syscall_semop ::
  CInt -> Ptr () -> CSize -> IO CInt

-- -----------------------------------------------------------------------------

syscall_semtimedop {- BROKEN -} :: CInt -> Ptr () -> CSize -> Ptr () -> IO CInt
syscall_semtimedop = undefined

foreign export ccall syscall_semtimedop ::
  CInt -> Ptr () -> CSize -> Ptr () -> IO CInt


