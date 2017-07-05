module HaLVM.POSIX.Fcntl(
         halvm_fcntl_setflags
       )
 where

import           Foreign.C.Types(CInt(..))
import           Foreign.Ptr(Ptr)

data FileLock = FileLock -- struct flock
data OwnerInfo = OwnerInfo -- struct f_owner_ex


halvm_fcntl_dupfd :: CInt -> CInt -> CInt -> IO CInt
halvm_fcntl_dupfd = undefined FileLock OwnerInfo

foreign export ccall halvm_fcntl_dupfd ::
  CInt -> CInt -> CInt -> IO CInt

halvm_fcntl_getflags :: CInt -> IO CInt
halvm_fcntl_getflags = undefined

foreign export ccall halvm_fcntl_getflags ::
  CInt -> IO CInt

halvm_fcntl_setflags :: CInt -> CInt -> IO CInt
halvm_fcntl_setflags = undefined

foreign export ccall halvm_fcntl_setflags ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getstatus :: CInt -> IO CInt
halvm_fcntl_getstatus = undefined

foreign export ccall halvm_fcntl_getstatus ::
  CInt -> IO CInt

halvm_fcntl_setstatus :: CInt -> CInt -> IO CInt
halvm_fcntl_setstatus = undefined

foreign export ccall halvm_fcntl_setstatus ::
  CInt -> CInt -> IO CInt

halvm_fcntl_setlock :: CInt -> Ptr FileLock -> CInt -> IO CInt
halvm_fcntl_setlock = undefined

foreign export ccall halvm_fcntl_setlock ::
  CInt -> Ptr FileLock -> CInt -> IO CInt

halvm_fcntl_getlock :: CInt -> Ptr FileLock -> IO CInt
halvm_fcntl_getlock = undefined

foreign export ccall halvm_fcntl_getlock ::
  CInt -> Ptr FileLock -> IO CInt

halvm_fcntl_ofd_setlock :: CInt -> Ptr FileLock -> CInt -> IO CInt
halvm_fcntl_ofd_setlock = undefined

foreign export ccall halvm_fcntl_ofd_setlock ::
  CInt -> Ptr FileLock -> CInt -> IO CInt

halvm_fcntl_ofd_getlock :: CInt -> Ptr FileLock -> IO CInt
halvm_fcntl_ofd_getlock = undefined

foreign export ccall halvm_fcntl_ofd_getlock ::
  CInt -> Ptr FileLock -> IO CInt

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

halvm_fcntl_getsig :: CInt -> IO CInt
halvm_fcntl_getsig = undefined

foreign export ccall halvm_fcntl_getsig ::
  CInt -> IO CInt

halvm_fcntl_setsig :: CInt -> CInt -> IO CInt
halvm_fcntl_setsig = undefined

foreign export ccall halvm_fcntl_setsig ::
  CInt -> CInt -> IO CInt

halvm_fcntl_setlease :: CInt -> CInt -> IO CInt
halvm_fcntl_setlease = undefined

foreign export ccall halvm_fcntl_setlease ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getlease :: CInt -> IO CInt
halvm_fcntl_getlease = undefined

foreign export ccall halvm_fcntl_getlease ::
  CInt -> IO CInt

halvm_fcntl_notify :: CInt -> CInt -> IO CInt
halvm_fcntl_notify = undefined

foreign export ccall halvm_fcntl_notify ::
  CInt -> CInt -> IO CInt

halvm_fcntl_setpipesize :: CInt -> CInt -> IO CInt
halvm_fcntl_setpipesize = undefined

foreign export ccall halvm_fcntl_setpipesize ::
  CInt -> CInt -> IO CInt

halvm_fcntl_getpipesize :: CInt -> IO CInt
halvm_fcntl_getpipesize = undefined

foreign export ccall halvm_fcntl_getpipesize ::
  CInt -> IO CInt

halvm_fcntl_add_seals :: CInt -> CInt -> IO CInt
halvm_fcntl_add_seals = undefined

foreign export ccall halvm_fcntl_add_seals ::
  CInt -> CInt -> IO CInt

halvm_fcntl_get_seals :: CInt -> IO CInt
halvm_fcntl_get_seals = undefined

foreign export ccall halvm_fcntl_get_seals ::
  CInt -> IO CInt

