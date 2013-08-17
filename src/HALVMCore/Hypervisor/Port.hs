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
       )
 where

import Control.Exception
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Generics

import Hypervisor.ErrorCodes
import Hypervisor.DomainInfo

-- |Communication Channel Endpoint
newtype Port = Port Word16
  deriving (Eq, Ord, Generic)

instance Show Port where
  show (Port x) = "echan:" ++ show x

instance Read Port where
  readsPrec d str =
    case splitAt 6 str of
      ("echan:",x) -> map (\ (p,rest) -> (Port p, rest)) (readsPrec d x)
      _            -> []

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
      error "ERROR: Tried to manufacture port with too large of an identifier."
  | otherwise = Port (fromIntegral x)

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
-- domain. May throw an ErrorCode on failure
allocPort :: DomId -> IO Port
allocPort remoteDomain = evtchn_alloc_unbound us them >>= standardPortReturn
 where
  us   = fromDomId domidSelf
  them = fromDomId remoteDomain

-- |Bind another domain's port, which they've shared with us.
bindRemotePort :: DomId -> Port -> IO Port
bindRemotePort d p = evtchn_bind_interdomain dom port >>= standardPortReturn
 where
  dom  = fromDomId d
  port = fromPort p

-- |Bind virtual IRQ  on virtual cpu to a port.
bindVirq :: Word32 -> Word32 -> IO Port
bindVirq virq vcpu = bind_virq virq vcpu >>= standardPortReturn

-- |Close an open port.
closePort :: Port -> IO ()
closePort (Port p) = evtchn_close (fromIntegral p) >>= standardUnitReturn

-- |Allocate a port on behalf of another domain, without causing the other
-- end to bind it. This is a privileged operation, and should only be called
-- if the underlying version of Xen has had the privilege check turned off.
-- The first argument is the domain this allocation is acting on behalf of.
-- The second is the domain it's allocating a port to.
allocUnboundPort :: DomId -> DomId -> IO Port
allocUnboundPort f t = evtchn_alloc_unbound fromp top >>= standardPortReturn
 where
  fromp = fromDomId f
  top   = fromDomId t

-- |Send an event on the given port.
sendOnPort :: Port -> IO ()
sendOnPort (Port p) = evtchn_send (fromIntegral p) >>= standardUnitReturn

-- |Perform the given operation within the IO monad with events on the
-- given port disabled.
withPortMasked :: Port -> IO a -> IO a
withPortMasked (Port p) a = do
  mask_evtchn (fromIntegral p)
  r <- a
  unmask_evtchn (fromIntegral p)
  return r

-- |Bind a physical IRQ to an event channel. The arguments are the IRQ to
-- map and a boolean stating whether or not the IRQ will be shared.
bindPhysicalIRQ :: Word32 -> Bool -> IO Port
bindPhysicalIRQ pirq_num share = bind_pirq pirq_num share >>= standardPortReturn

standardUnitReturn :: Integral a => a -> IO ()
standardUnitReturn x
  | x < 0     = throwXenError x
  | otherwise = return ()

standardPortReturn :: Integral a => a -> IO Port
standardPortReturn x
  | x < 0     = throwXenError x
  | x >= 1024 = throw ENOBUFS
  | otherwise = return (Port (fromIntegral x))

throwXenError :: Integral a => a -> IO b
throwXenError x = throw errorCode
  where
   errorCode :: ErrorCode
   errorCode = toEnum (fromIntegral (-x))

--
-- --------------------------------------------------------------------------
--

foreign import ccall unsafe "events.h set_haskell_handler" 
  set_port_handler :: Word32 -> StablePtr(IO()) -> IO ()

foreign import ccall unsafe "events.h clear_haskell_handler" 
  unset_port_handler :: Word32 -> IO (StablePtr(IO ()))

foreign import ccall unsafe "events.h channel_alloc" 
  evtchn_alloc_unbound :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h bind_virq" 
  bind_virq :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h channel_bind" 
  evtchn_bind_interdomain :: Word32 -> Word32 -> IO Int32

foreign import ccall unsafe "events.h channel_close" 
  evtchn_close :: Word32 -> IO Int32

foreign import ccall unsafe "events.h channel_send" 
  evtchn_send :: Word32 -> IO Int32

foreign import ccall unsafe "events.h mask_channel" 
  mask_evtchn :: Word32 -> IO ()

foreign import ccall unsafe "events.h unmask_channel" 
  unmask_evtchn :: Word32 -> IO ()

foreign import ccall unsafe "events.h bind_pirq" 
  bind_pirq :: Word32 -> Bool -> IO Int32
