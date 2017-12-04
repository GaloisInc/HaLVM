module HaLVM.POSIX.Fcntl(
         halvm_fcntl_setflags
       )
 where

#include <bits/fcntl.h>
#include <fcntl.h>

import           Data.Bits(complement, (.|.), (.&.))
import           Foreign.C.Error(eINVAL)
import           Foreign.C.Types(CInt(..))
import           Foreign.Ptr(Ptr)
import           HaLVM.POSIX.Errno(errnoReturn)
import           HaLVM.POSIX.FileDescriptors(DescriptorEntry(..), dup,
                                             withFileDescriptorEntry_,
                                             withFileDescriptorEntry)

data FileLock = FileLock -- struct flock
data OwnerInfo = OwnerInfo -- struct f_owner_ex

halvm_fcntl_dupfd :: CInt -> CInt -> CInt -> IO CInt
halvm_fcntl_dupfd fd arg flags
  | fd < 0    = errnoReturn eINVAL
  | otherwise =
      dup (>= (fromIntegral arg)) (fromIntegral fd) Nothing flags

foreign export ccall halvm_fcntl_dupfd ::
  CInt -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_getflags :: CInt -> IO CInt
halvm_fcntl_getflags fd =
  withFileDescriptorEntry_ (fromIntegral fd) $ \ ent ->
    if descCloseOnExec ent
       then return (#const FD_CLOEXEC)
       else return 0

foreign export ccall halvm_fcntl_getflags ::
  CInt -> IO CInt

halvm_fcntl_setflags :: CInt -> CInt -> IO CInt
halvm_fcntl_setflags fd flag
  | (flag /= (#const FD_CLOEXEC)) && (flag /= 0) = errnoReturn eINVAL
  | otherwise =
      withFileDescriptorEntry (fromIntegral fd) $ \ ent ->
        return (ent{ descCloseOnExec = (flag == (#const FD_CLOEXEC)) }, 0)

foreign export ccall halvm_fcntl_setflags ::
  CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_getstatus :: CInt -> IO CInt
halvm_fcntl_getstatus fd =
  withFileDescriptorEntry_ (fromIntegral fd) $ \ ent ->
    return (descStatusFlags ent)

foreign export ccall halvm_fcntl_getstatus ::
  CInt -> IO CInt

halvm_fcntl_setstatus :: CInt -> CInt -> IO CInt
halvm_fcntl_setstatus fd newFlags =
  withFileDescriptorEntry (fromIntegral fd) $ \ ent ->
    do let otherFlags = descStatusFlags ent .&.
                            (complement flagsLinuxSupportsSetting)
           newFlags' = newFlags .&. flagsLinuxSupportsSetting
           resultFlags = otherFlags .|. newFlags'
       -- FIXME: Actually tell the upstream implementations about this?
       return (ent{ descStatusFlags = resultFlags }, 0)

flagsLinuxSupportsSetting :: CInt
flagsLinuxSupportsSetting =
  (#const O_APPEND)   .|. (#const O_ASYNC)   .|.
  (#const O_DIRECT)   .|. (#const O_NOATIME) .|.
  (#const O_NONBLOCK)

foreign export ccall halvm_fcntl_setstatus ::
  CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_setlock :: CInt -> Ptr FileLock -> CInt -> IO CInt
halvm_fcntl_setlock = undefined

foreign export ccall halvm_fcntl_setlock ::
  CInt -> Ptr FileLock -> CInt -> IO CInt

halvm_fcntl_getlock :: CInt -> Ptr FileLock -> IO CInt
halvm_fcntl_getlock = undefined

foreign export ccall halvm_fcntl_getlock ::
  CInt -> Ptr FileLock -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_ofd_setlock :: CInt -> Ptr FileLock -> CInt -> IO CInt
halvm_fcntl_ofd_setlock = undefined

foreign export ccall halvm_fcntl_ofd_setlock ::
  CInt -> Ptr FileLock -> CInt -> IO CInt

halvm_fcntl_ofd_getlock :: CInt -> Ptr FileLock -> IO CInt
halvm_fcntl_ofd_getlock = undefined

foreign export ccall halvm_fcntl_ofd_getlock ::
  CInt -> Ptr FileLock -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_getowner :: CInt -> IO CInt
halvm_fcntl_getowner = undefined

foreign export ccall halvm_fcntl_getowner ::
  CInt -> IO CInt

halvm_fcntl_setowner :: CInt -> CInt -> IO CInt
halvm_fcntl_setowner = undefined

foreign export ccall halvm_fcntl_setowner ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getowner_ex :: CInt -> Ptr OwnerInfo -> IO CInt
halvm_fcntl_getowner_ex = undefined

foreign export ccall halvm_fcntl_getowner_ex ::
  CInt -> Ptr OwnerInfo -> IO CInt

halvm_fcntl_setowner_ex :: CInt -> Ptr OwnerInfo -> IO CInt
halvm_fcntl_setowner_ex = undefined

foreign export ccall halvm_fcntl_setowner_ex ::
  CInt -> Ptr OwnerInfo -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_getsig :: CInt -> IO CInt
halvm_fcntl_getsig = undefined

foreign export ccall halvm_fcntl_getsig ::
  CInt -> IO CInt

halvm_fcntl_setsig :: CInt -> CInt -> IO CInt
halvm_fcntl_setsig = undefined

foreign export ccall halvm_fcntl_setsig ::
  CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_setlease :: CInt -> CInt -> IO CInt
halvm_fcntl_setlease = undefined

foreign export ccall halvm_fcntl_setlease ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getlease :: CInt -> IO CInt
halvm_fcntl_getlease = undefined

foreign export ccall halvm_fcntl_getlease ::
  CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_notify :: CInt -> CInt -> IO CInt
halvm_fcntl_notify = undefined

foreign export ccall halvm_fcntl_notify ::
  CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_setpipesize :: CInt -> CInt -> IO CInt
halvm_fcntl_setpipesize = undefined

foreign export ccall halvm_fcntl_setpipesize ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getpipesize :: CInt -> IO CInt
halvm_fcntl_getpipesize = undefined

foreign export ccall halvm_fcntl_getpipesize ::
  CInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_fcntl_add_seals :: CInt -> CInt -> IO CInt
halvm_fcntl_add_seals = undefined

foreign export ccall halvm_fcntl_add_seals ::
  CInt -> CInt -> IO CInt

halvm_fcntl_get_seals :: CInt -> IO CInt
halvm_fcntl_get_seals = undefined

foreign export ccall halvm_fcntl_get_seals ::
  CInt -> IO CInt

