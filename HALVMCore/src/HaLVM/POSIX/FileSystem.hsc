module HaLVM.POSIX.FileSystem()
 where

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

import Control.Concurrent.MVar(MVar,newMVar,withMVar)
import Data.Bits((.&.))
import Foreign.C.String(peekCAString)
import Foreign.C.Types(CChar(..),CInt(..),CUInt(..))
import Foreign.Ptr(Ptr)
import HaLVM.POSIX.Errno(errnoReturn)
import HaLVM.POSIX.FileDescriptors(DescriptorEntry(..),DescriptorType(..))
import HaLVM.POSIX.FileDescriptors(addFd)
import HaLVM.FileSystem(FileSystem,ModeBits(..),OpenFlags(..),init,open)
import Prelude hiding (init)
import System.IO.Unsafe(unsafePerformIO)
import System.Posix.Types(CMode(..))

-- -----------------------------------------------------------------------------

{-# NOINLINE mFileSystem #-}
mFileSystem :: MVar FileSystem
mFileSystem = unsafePerformIO $ init >>= newMVar

-- -----------------------------------------------------------------------------

halvm_syscall_open :: Ptr CChar -> CInt -> CMode -> IO CInt
halvm_syscall_open cstr flags mmode
  | flags .&. (#const O_PATH) /= 0 = undefined -- FIXME/BROKEN
  | otherwise =
      do str <- peekCAString cstr
         res <- withMVar mFileSystem $ \ fs -> open fs str flags'
         case res of
           Left errno -> errnoReturn errno
           Right val  ->
             fromIntegral `fmap` addFd DescriptorEntry {
                descCloseOnExec = flags .&. (#const O_CLOEXEC) /= 0,
                descStatusFlags = 0,
                descType        = DescFile val
             }
 where
  maddFlag' x bitval bits rest | x .&. bitval /= 0 = bits ++ rest
                               | otherwise         = rest
  maddFlag  x bitval flag rest = maddFlag' x bitval [flag] rest
  --
  flags' = maddFlag  flags (#const O_APPEND)     OpenAppend                    $
           maddFlag  flags (#const O_ASYNC)      OpenAsync                     $
           maddFlag  flags (#const O_CLOEXEC)    OpenCloseOnExec               $
           maddFlag  flags (#const O_CREAT)     (OpenCreate modes)             $
           maddFlag  flags (#const O_DIRECT)     OpenDirect                    $
           maddFlag  flags (#const O_DIRECTORY)  OpenDirectory                 $
           maddFlag  flags (#const O_DSYNC)      OpenDSync                     $
           maddFlag  flags (#const O_EXCL)       OpenExclude                   $
           maddFlag  flags (#const O_LARGEFILE)  OpenLargeFile                 $
           maddFlag  flags (#const O_NOATIME)    OpenNoAccessTime              $
           maddFlag  flags (#const O_NOCTTY)     OpenNoControlTTY              $
           maddFlag  flags (#const O_NOFOLLOW)   OpenNoFollow                  $
           maddFlag  flags (#const O_NONBLOCK)   OpenNonBlock                  $
           maddFlag  flags (#const O_NDELAY)     OpenNonBlock                  $
           maddFlag  flags (#const O_SYNC)       OpenSync                      $
           maddFlag  flags (#const O_TMPFILE)   (OpenTempFile modes)           $
           maddFlag  flags (#const O_TRUNC)      OpenTruncate                 []
  modes = maddFlag' mmode (#const S_IRWXU) [UserRead,UserWrite,UserExec]       $
          maddFlag  mmode (#const S_IRUSR) UserRead                            $
          maddFlag  mmode (#const S_IWUSR) UserWrite                           $
          maddFlag  mmode (#const S_IXUSR) UserExec                            $
          maddFlag' mmode (#const S_IRWXG) [GroupRead,GroupWrite,GroupExec]    $
          maddFlag  mmode (#const S_IRGRP) GroupRead                           $
          maddFlag  mmode (#const S_IWGRP) GroupWrite                          $
          maddFlag  mmode (#const S_IXGRP) GroupExec                           $
          maddFlag' mmode (#const S_IRWXO) [OthersRead,OthersWrite,OthersExec] $
          maddFlag  mmode (#const S_IROTH) OthersRead                          $
          maddFlag  mmode (#const S_IWOTH) OthersWrite                         $
          maddFlag  mmode (#const S_IXOTH) OthersExec                          $
          maddFlag  mmode (#const S_ISUID) SetUID                              $
          maddFlag  mmode (#const S_ISGID) SetGID                              $
          maddFlag  mmode (#const S_ISVTX) Sticky                             []

foreign export ccall halvm_syscall_open ::
  Ptr CChar -> CInt -> CMode -> IO CInt

-- -----------------------------------------------------------------------------

halvm_syscall_getdents {- BROKEN -} :: CUInt -> Ptr CUInt -> CUInt -> IO CInt
halvm_syscall_getdents = undefined

foreign export ccall halvm_syscall_getdents ::
  CUInt -> Ptr CUInt -> CUInt -> IO CInt

-- -----------------------------------------------------------------------------

halvm_syscall_getdents64 {- BROKEN -} :: CUInt -> Ptr CUInt -> CUInt -> IO CInt
halvm_syscall_getdents64 = undefined

foreign export ccall halvm_syscall_getdents64 ::
  CUInt -> Ptr CUInt -> CUInt -> IO CInt

