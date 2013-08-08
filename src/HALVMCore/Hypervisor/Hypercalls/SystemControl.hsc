-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Hypercalls.SystemControl(
         SystemControlOp(..)
       , systemControlOp
       , buildReadConsoleCommand
       , buildGetDomInfoListReq
       , readGetDomInfoListResp
       )
 where

import Control.Exception
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.ErrorCodes
import Hypervisor.Structures.DomainInfo

#include <stdint.h>
#include <sys/types.h>
#define __XEN_TOOLS__
#include <xen/sysctl.h>

data SystemControlOp = SysCtlReadConsole
                     | SysCtlPhysicalInfo
                     | SysCtlGetSchedulerId
                     | SysCtlGetDomInfoList

scCmdVal :: SystemControlOp -> Word32
scCmdVal SysCtlReadConsole    = (#const XEN_SYSCTL_readconsole)
scCmdVal SysCtlPhysicalInfo   = (#const XEN_SYSCTL_physinfo)
scCmdVal SysCtlGetSchedulerId = (#const XEN_SYSCTL_sched_id)
scCmdVal SysCtlGetDomInfoList = (#const XEN_SYSCTL_getdomaininfolist)

systemControlOp :: SystemControlOp      ->
                   (Ptr a -> IO b)      ->
                   (b -> Ptr a -> IO c) ->
                   IO c
systemControlOp op setter getter =
  bracket (mallocBytes (#size xen_sysctl_t)) free $ \ buffer -> do
    bzero buffer (#size xen_sysctl_t)
    (#poke xen_sysctl_t, cmd)               buffer (scCmdVal op)
    (#poke xen_sysctl_t, interface_version) buffer
          ((#const XEN_SYSCTL_INTERFACE_VERSION) :: Word32)
    let argp = buffer `plusPtr` (#offset xen_sysctl_t,u)
    setterres <- setter argp
    initres   <- do_sysctl_op buffer
    if initres == 0
      then getter setterres argp
      else throw (toEnum (-initres) :: ErrorCode)

buildReadConsoleCommand :: Bool -> Word8 -> Word32 -> Word32 -> Ptr a ->
                           Ptr b ->
                           IO ()
buildReadConsoleCommand clear incr index bufsize buffer ptr = do
  (#poke xen_sysctl_readconsole_t,clear) ptr (if clear then 1 else 0 :: Word8)
  (#poke xen_sysctl_readconsole_t,incremental) ptr incr
  (#poke xen_sysctl_readconsole_t,index) ptr index
  (#poke xen_sysctl_readconsole_t,buffer) ptr buffer
  (#poke xen_sysctl_readconsole_t,count)  ptr bufsize

buildGetDomInfoListReq :: Word16 -> Word32 -> Ptr a ->
                          IO (Ptr DomainInfo)
buildGetDomInfoListReq first num ptr = do
  buffer <- mallocArray (fromIntegral num)
  -- FIXME: This method (freeing on the read, later) leaks memory when an
  -- exception is raised before the read happens.
  (#poke xen_sysctl_getdomaininfolist_t,first_domain) ptr first
  (#poke xen_sysctl_getdomaininfolist_t,max_domains)  ptr num
  (#poke xen_sysctl_getdomaininfolist_t,buffer)       ptr buffer
  return buffer

readGetDomInfoListResp :: Ptr DomainInfo -> Ptr a -> IO [DomainInfo]
readGetDomInfoListResp bufferp respp = do
  num <- (#peek xen_sysctl_getdomaininfolist_t,num_domains) respp :: IO Word32
  res <- peekArray (fromIntegral num) bufferp
  free bufferp
  return res

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word -> IO ()

foreign import ccall unsafe "hypercalls.h HYPERCALL_sysctl"
  do_sysctl_op :: Ptr a -> IO Int

