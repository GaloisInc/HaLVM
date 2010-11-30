-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- blank line for Haddock/hsc2hs
-- |Low-level interface for Xenbus access, used by both Xenstore clients and the Xenstore server.
module XenDevice.XenbusInterface(Interface(..), interface, IdxPtr, canWrite, canRead, xbWrite, xbRead)
    where

import Foreign.Ptr(Ptr,plusPtr)
import Data.Word(Word32)
import Foreign.C.Types(CChar)
import Hypervisor.EventWaitSet(EventWaitSet,newWaitSet)
import Hypervisor.Memory(VPtr)
import Hypervisor.Port(Port)

#include <errno.h>
#include "types.h"
#include "xen/io/xs_wire.h"

-- | Low-level interface to a Xenbus page and port
data Interface = Interface{ rsp_prodp :: IdxPtr
                          , rsp_consp :: IdxPtr
                          , rsp_shdata :: Ptr CChar
                          , req_prodp :: IdxPtr
                          , req_consp :: IdxPtr
                          , req_shdata :: Ptr CChar
                          , waitSet :: EventWaitSet
                          }

-- | Pointers to index counters in a Xenbus interface.
type IdxPtr = Ptr Word32

-- | Create an interface given a page address and a port.
interface :: VPtr a -> Port -> IO Interface
interface intf port = 
  do ws <- newWaitSet port
     return Interface{ rsp_prodp = intf `plusPtr` #offset struct xenstore_domain_interface, rsp_prod
                     , rsp_consp = intf `plusPtr` #offset struct xenstore_domain_interface, rsp_cons
                     , rsp_shdata = intf `plusPtr` #offset struct xenstore_domain_interface, rsp
                     , req_prodp = intf `plusPtr` #offset struct xenstore_domain_interface, req_prod
                     , req_consp = intf `plusPtr` #offset struct xenstore_domain_interface, req_cons
                     , req_shdata = intf `plusPtr` #offset struct xenstore_domain_interface, req
                     , waitSet = ws
                     }

canWrite :: IdxPtr -> IdxPtr -> IO Bool
canWrite = xb_can_write

canRead :: IdxPtr -> IdxPtr -> IO Bool
canRead = xb_can_read 

xbWrite :: IdxPtr -> IdxPtr -> Ptr CChar ->
           Port -> Ptr CChar -> Int ->
           IO Int
xbWrite = xb_write

xbRead :: IdxPtr -> IdxPtr -> Ptr CChar ->
          Port -> Ptr CChar -> Int ->
          IO Int
xbRead = xb_read

foreign import ccall unsafe "xenbus.h xb_write" xb_write ::  IdxPtr -> IdxPtr -> Ptr CChar -> Port -> Ptr CChar -> Int -> IO Int
foreign import ccall unsafe "xenbus.h xb_read" xb_read  :: IdxPtr -> IdxPtr -> Ptr CChar -> Port -> Ptr CChar -> Int -> IO Int
foreign import ccall unsafe "xenbus.h xb_can_write" xb_can_write  :: IdxPtr -> IdxPtr -> IO Bool
foreign import ccall unsafe "xenbus.h xb_can_read" xb_can_read :: IdxPtr -> IdxPtr -> IO Bool
