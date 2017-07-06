module HaLVM.POSIX.Hostname(getDomainName,setDomainName) where

#define _GNU_SOURCE
#include <sys/utsname.h>

import Control.Concurrent.MVar(MVar,newMVar,readMVar,modifyMVar_,withMVar)
import Foreign.C.String(peekCStringLen,withCStringLen)
import Foreign.C.Types(CInt(..),CChar(..),CSize(..))
import Foreign.Marshal.Utils(copyBytes,fillBytes)
import Foreign.Ptr(Ptr,plusPtr)
import System.IO.Unsafe(unsafePerformIO)

{-# NOINLINE mDomainName #-}
mDomainName :: MVar String
mDomainName = unsafePerformIO $ newMVar "Sweet.HaLVM"

getDomainName :: IO String
getDomainName = readMVar mDomainName

setDomainName :: String -> IO ()
setDomainName x = modifyMVar_ mDomainName (const (return x))

syscall_getdomainname :: Ptr CChar -> CSize -> IO CInt
syscall_getdomainname ptr maxsize =
  withMVar mDomainName $ \ str ->
    withCStringLen str $ \ (from, strlen) ->
      do let copyAmt = min (fromIntegral maxsize) strlen
         fillBytes ptr 0 (fromIntegral maxsize)
         copyBytes ptr from copyAmt
         return 0

foreign export ccall syscall_getdomainname ::
  Ptr CChar -> CSize -> IO CInt

syscall_setdomainname :: Ptr CChar -> CSize -> IO CInt
syscall_setdomainname ptr size =
  do setDomainName =<< peekCStringLen (ptr, fromIntegral size)
     return 0

foreign export ccall syscall_setdomainname ::
  Ptr CChar -> CSize -> IO CInt

syscall_uname :: Ptr CInt -> IO CInt
syscall_uname ptr =
  do fillBytes ptr 0 (#size struct utsname)
     withCStringLen "HaLVM" $ \ (strptr, strlen) ->
       copyBytes ((#ptr struct utsname,sysname) ptr) strptr strlen
     --
     withMVar mDomainName $ \ hostname ->
       withCStringLen hostname $ \ (strptr, strlen) ->
         do let copylen = min (#size ((struct utsname*)0)->nodename) strlen
            copyBytes ((#ptr struct utsname,nodename) ptr) strptr copylen
            copyBytes ((#ptr struct utsname,domainname) ptr) strptr copylen
     --
     withCStringLen "3.0.0" $ \ (strptr, strlen) ->  -- FIXME
       copyBytes ((#ptr struct utsname,release) ptr) strptr strlen
     --
     withCStringLen "HaLVM 3.0.0 (x86_64/KVM)" $ \ (strptr, strlen) ->  -- FIXME
       copyBytes ((#ptr struct utsname,version) ptr) strptr strlen
     --
     withCStringLen "x86_64" $ \ (strptr, strlen) ->  -- FIXME
       copyBytes ((#ptr struct utsname,machine) ptr) strptr strlen
     --
     return 0

foreign export ccall syscall_uname ::
  Ptr CInt -> IO CInt
