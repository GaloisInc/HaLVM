-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Andrew Tolmach <apt@galois.com> and Adam Wick <awick@galois.com>
-- BANNEREND
-- |Utilities for dealing with Xen event channels, which the HALVM calls
-- ports. 
module Hypervisor.Port(
         Port
       , toPort, fromPort
       , setPortHandler
       , unsetPortHandler
       , allocPort
       , bindRemotePort
       , closePort
       , allocUnboundPort
       , sendOnPort
       , withPortMasked
       , bindVirq
       , bindPhysicalIRQ
       , irqSendEoi
       , irqNeedsEoi
       , irqShared
       )
 where

import Data.Bits (Bits(testBit))
import Data.Generics(Typeable, Data)
import Data.Int
import Data.Word
import Data.Word10
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable(Storable)
import Hypervisor.Basics

-- |Communication Channel Endpoint
newtype Port = Port Word10 -- ^ allows 1024 channel endpoints per domain
  deriving (Eq, Ord, Show, Read, Storable, Typeable, Data)

-- |Convert a port into its identifying number.
fromPort :: Integral a => Port -> a
fromPort (Port p) = fromIntegral p

-- |Given a number, convert it into a port. Note that ports run in a limited
-- series of values, and that manufacturing your own port is a really bad
-- idea.
toPort :: Integral a => a -> Port
toPort x 
  | fromIntegral x < (0::Integer) =
      error "ERROR: Tried to manufacture port with negative identifier."
  | fromIntegral x >= (1024::Integer) =
      error "ERROR: Tried to manufacture port with too-high identifier."
  | otherwise =
      Port (fromIntegral x)

-- |Install a function that will be invoked whenever the HALVM receives
-- an event on the given channel.
setPortHandler :: Port -> IO () -> IO ()
setPortHandler (Port p) h = do
  sptr <- newStablePtr h
  set_port_handler (fromIntegral p) sptr

-- |Remove the current event handler on an event channel, returning it.
-- This can be particularly useful when black-boxed components may need
-- to chain together a series of event handlers. Calling this routine is
-- safe even if no previous handler has been assigned to the port; in
-- that case, the return value will be a noop.
unsetPortHandler :: Port -> IO (IO ())
unsetPortHandler (Port p) = do
  sptr <- unset_port_handler (fromIntegral p)
  if castStablePtrToPtr sptr == nullPtr
     then return (return ())
     else do retval <- deRefStablePtr sptr
             freeStablePtr sptr
             return retval

-- |Allocate a fresh event channel that will be connected to by the given
-- domain.
allocPort :: DomId -> Xen Port
allocPort (DomId remoteDomain) = do 
  let (DomId domSelf) = domidSelf
  res <- evtchn_alloc_unbound (fromIntegral domSelf) (fromIntegral remoteDomain)
  case () of
             () | res < 0    -> toError res
                | res < 1024 -> return $ Port (fromIntegral res)
                | otherwise  -> error "INTERNAL ERROR: Bad port! (allocPort)"

-- |Bind another domain's port, which they've shared with us.
bindRemotePort :: DomId -> Port -> Xen Port
bindRemotePort (DomId remoteDomain) (Port remotePort) = do 
  res <- evtchn_bind_interdomain (fromIntegral remoteDomain) (fromIntegral remotePort)
  case () of
             () | res < 0    -> toError res
                | res < 1024 -> return $ Port (fromIntegral res)
                | otherwise  -> error $ "INTERNAL ERROR: Bad port! (bindRemote)"

-- |Bind virtual IRQ  on virtual cpu to a port.
bindVirq :: Word32 -> Word32 -> Xen Port
bindVirq virq vcpu = do
  res <- bind_virq virq vcpu
  case () of
             () | res < 0    -> toError res
                | res < 1024 -> return $ Port (fromIntegral res)
                | otherwise  -> error "INTERNAL ERROR: Bad port! (bindVirq)"

-- |Close an open port.
closePort :: Port -> Xen ()
closePort (Port p) = do
  res <- evtchn_close (fromIntegral p)
  case res of
             0 -> return $ ()
             _ -> toError res

-- |Allocate a port on behalf of another domain, without causing the other
-- end to bind it. This is a privileged operation, and should only be called
-- if the underlying version of Xen has had the privilege check turned off.
-- The first argument is the domain this allocation is acting on behalf of.
-- The second is the domain it's allocating a port to.
allocUnboundPort :: DomId -> DomId -> Xen Port
allocUnboundPort (DomId fromDom) (DomId toDom) = do
  res <- evtchn_alloc_unbound (fromIntegral fromDom) (fromIntegral toDom)
  case () of
             () | res < 0    -> toError res
                | res < 1024 -> return $ Port (fromIntegral res)
                | otherwise  -> error "INTERNAL ERROR: Bad port! (allocUnbound)"
	  
-- |Send an event on the given port.
sendOnPort :: Port -> Xen ()
sendOnPort (Port p) = do
  res <- evtchn_send (fromIntegral p)
  case () of
             () | res == 0  -> return $ ()
                | otherwise -> toError res

-- |Perform the given operation within the IO monad with events on the
-- given port disabled.
withPortMasked :: Port -> IO a -> IO a
withPortMasked (Port p) a = 
    do mask_evtchn (fromIntegral p)
       r <- a
       unmask_evtchn (fromIntegral p)
       return r

-- |Bind a physical IRQ to an event channel. The arguments are the IRQ to
-- map and a boolean stating whether or not the IRQ will be shared.
bindPhysicalIRQ :: Word32 -> Bool -> Xen Port
bindPhysicalIRQ pirq_num share = do
  res <- bind_pirq pirq_num share
  case () of
             () | res < 0    -> toError res
                | res < 1024 -> return $ Port (fromIntegral res)
                | otherwise  -> error "INTERNAL ERROR: Bad port! (bindPhysIRQ)"


toError :: Integral a => a -> IO b
toError x = xThrow $ toEnum $ fromIntegral (-x)

irqSendEoi :: Word32 -> Xen ()
irqSendEoi irq = irq_send_eoi irq

irqNeedsEoi :: Word32 -> Xen Bool
irqNeedsEoi irq = do
  status <- irq_get_status irq
  return (testBit status 0)

irqShared :: Word32 -> Xen Bool
irqShared irq = do
  status <- irq_get_status irq
  return (testBit status 1)

--
-- --------------------------------------------------------------------------
--

foreign import ccall unsafe "events.h set_port_handler" 
  set_port_handler :: Word32 -> StablePtr(IO()) -> IO ()

foreign import ccall unsafe "events.h unset_port_handler" 
  unset_port_handler :: Word32 -> IO (StablePtr(IO ()))

foreign import ccall unsafe "events.h evtchn_alloc_unbound" 
  evtchn_alloc_unbound :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h bind_virq" 
  bind_virq :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h evtchn_bind_interdomain" 
  evtchn_bind_interdomain :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h evtchn_close" 
  evtchn_close :: Word32 -> IO Int32

foreign import ccall unsafe "events.h evtchn_send" 
  evtchn_send :: Word32 -> IO Int32

foreign import ccall unsafe "events.h mask_evtchn" 
  mask_evtchn :: Word32 -> IO ()

foreign import ccall unsafe "events.h unmask_evtchn" 
  unmask_evtchn :: Word32 -> IO ()

foreign import ccall unsafe "events.h bind_pirq" 
  bind_pirq :: Word32 -> Bool -> IO Int32

foreign import ccall unsafe "events.h irq_get_status"
  irq_get_status :: Word32 -> IO Word32

foreign import ccall unsafe "events.h irq_send_eoi"
  irq_send_eoi :: Word32 -> IO ()
