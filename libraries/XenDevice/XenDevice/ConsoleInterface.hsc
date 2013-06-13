-- blank line for Haddock/hsc2hs
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- |Low-level interface for console access, used by both clients and servers.
module XenDevice.ConsoleInterface(
         Interface(..), IdxPtr
       , interface, backendInterface
       , readConsole, writeConsole
       , canReadConsole, canWriteConsole
       , getConsolePort, getConsoleMFN
       )
    where

import Data.Word(Word32,Word)
import Foreign.C.Types(CChar)
import Foreign.C.String(CString)
import Foreign.Ptr(Ptr,plusPtr)
import Hypervisor.EventWaitSet(EventWaitSet,newWaitSet)
import Hypervisor.Memory(MFN, VPtr, toMFN)
import Hypervisor.Port(Port,fromPort,toPort)

#include <xen/io/console.h>

-- | Low-level interface to a console page and port
data Interface = Interface{ in_prodp :: IdxPtr
                          , in_consp :: IdxPtr
                          , in_shdata :: Ptr CChar
                          , out_prodp :: IdxPtr
                          , out_consp :: IdxPtr
                          , out_shdata :: Ptr CChar
                          , in_ring_size :: Int
                          , out_ring_size :: Int
                          , waitSet :: EventWaitSet
                          , interface_port :: Port
                          }

-- | Pointers to index counters in a console interface.
type IdxPtr = Ptr Word32

-- | Create an interface given a page address and port.
interface :: VPtr a -> Port -> IO Interface
interface intf port = 
  do ws <- newWaitSet port
     return Interface{ 
       in_prodp = intf `plusPtr` #offset struct xencons_interface, in_prod
     , in_consp = intf `plusPtr` #offset struct xencons_interface, in_cons
     , in_shdata = intf `plusPtr` #offset struct xencons_interface, in
     , out_prodp = intf `plusPtr` #offset struct xencons_interface, out_prod
     , out_consp = intf `plusPtr` #offset struct xencons_interface, out_cons
     , out_shdata = intf `plusPtr` #offset struct xencons_interface, out
     , in_ring_size = inbufSize
     , out_ring_size = outbufSize
     , waitSet = ws
     , interface_port = port
     }

-- | Create a backend interface given a page address and port
backendInterface :: VPtr a -> Port -> IO Interface
backendInterface intf port =
  do ws <- newWaitSet port
     return Interface{ 
       in_prodp = intf `plusPtr` #offset struct xencons_interface, out_prod
     , in_consp = intf `plusPtr` #offset struct xencons_interface, out_cons
     , in_shdata = intf `plusPtr` #offset struct xencons_interface, out
     , out_prodp = intf `plusPtr` #offset struct xencons_interface, in_prod
     , out_consp = intf `plusPtr` #offset struct xencons_interface, in_cons
     , out_shdata = intf `plusPtr` #offset struct xencons_interface, in
     , in_ring_size = outbufSize
     , out_ring_size = inbufSize
     , waitSet = ws
     , interface_port = port
     }

canWriteConsole :: IdxPtr -> IdxPtr -> Int -> IO Bool
canWriteConsole = xencons_can_send

canReadConsole :: IdxPtr -> IdxPtr -> IO Bool
canReadConsole = xencons_can_receive

writeConsole :: IdxPtr -> IdxPtr -> Ptr CChar -> 
                Port -> Int -> CString -> Int -> 
                IO Int
writeConsole i1 i2 s p c x = xencons_ring_send i1 i2 s (fromPort p) c x

readConsole :: IdxPtr -> IdxPtr -> Ptr CChar -> 
               Port -> Int -> CString -> Int -> 
               IO Int
readConsole i1 i2 s1 p a s2 b= xencons_ring_receive i1 i2 s1 (fromPort p) a s2 b

getConsolePort :: IO Port
getConsolePort = toPort `fmap` get_console_evtchn

getConsoleMFN :: IO MFN
getConsoleMFN = (toMFN . fromIntegral) `fmap` get_console_mfn

inbufSize :: Int
inbufSize = #const ({ struct xencons_interface s; sizeof s.in; })

outbufSize :: Int
outbufSize = #const ({ struct xencons_interface s; sizeof s.out; })

--
-- --------------------------------------------------------------------------
--

foreign import ccall unsafe "console.h xencons_ring_send" 
  xencons_ring_send :: IdxPtr -> IdxPtr -> Ptr CChar -> 
                       Word32 -> Int -> CString -> Int -> 
                       IO Int
foreign import ccall unsafe "console.h xencons_ring_receive" 
  xencons_ring_receive :: IdxPtr -> IdxPtr -> Ptr CChar -> 
                          Word32 -> Int -> CString -> Int -> 
                          IO Int
foreign import ccall unsafe "console.h get_console_evtchn" 
  get_console_evtchn :: IO Word32
foreign import ccall unsafe "console.h get_console_mfn" 
  get_console_mfn :: IO Word
foreign import ccall unsafe "console.h xencons_can_send" 
  xencons_can_send :: IdxPtr -> IdxPtr -> Int -> IO Bool
foreign import ccall unsafe "console.h xencons_can_receive" 
  xencons_can_receive :: IdxPtr -> IdxPtr -> IO Bool
