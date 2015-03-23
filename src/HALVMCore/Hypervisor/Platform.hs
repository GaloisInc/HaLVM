-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Various routines for interacting with the underlying hardware platform.
module Hypervisor.Platform(
         readEmergencyConsole
       , hostPhysicalInfo
       , currentSchedulerId
       , setIRQPermission
       , setIOPortPermission
       , setIOMemoryPermission
       , setIOPrivilegeLevel
       , sendEOI
       , domainList
       )
 where

import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable
import Hypervisor.DomainInfo
import Hypervisor.Hypercalls
import Hypervisor.Memory
import Hypervisor.Structures.PhysicalInfo

-- |Read from the emergency console (also known as the Xen buffer ring, or
-- other names). If the first argument is True, clears the buffer after
-- reading. If specified, the pair of integers specifies the first index
-- to read and the amount to read, respectively.
readEmergencyConsole :: Bool -> Maybe (Int,Int) -> IO String
readEmergencyConsole clear mpartial =
  allocaBytes len $ \ buffer -> do
    systemControlOp SysCtlReadConsole
      (buildReadConsoleCommand clear incr (fromIntegral index) bufsize buffer)
      (\ _ _ -> peekCAStringLen (buffer, len))
 where
  len = fromIntegral bufsize
  (incr, index, bufsize) = case mpartial of
                             Nothing    -> (0, 0, (64 * 1024))
                             Just (i,s) -> (1, i, fromIntegral s)

-- |Get some basic information about the underlying physical platform.
hostPhysicalInfo :: IO HostPhysicalInfo
hostPhysicalInfo =
  systemControlOp SysCtlPhysicalInfo (\ _ -> return ()) (\ _ p -> peek p)

-- |Get the ID of the current scheduler.
currentSchedulerId :: IO Word32
currentSchedulerId  =
  systemControlOp SysCtlGetSchedulerId (\ _ -> return ()) (\ _ p -> peek p)

-- |Set whether or not the given domain is allowed to receive the given
-- physical IRQ.
setIRQPermission :: DomId -> Word8 -> Bool -> IO ()
setIRQPermission dom pirq allow =
  domainControlOp DomCtlSetIRQPerm (fromDomId dom)
    (buildIRQPermissionReq pirq allow)
    (\ _ _ _ -> return ())

-- |Set whether or not the given domain is allowed to access the given IO
-- ports. The arguments are the port and how many ports after that (inclusive)
-- the domain should or shouldn't be allowed to access
setIOPortPermission :: DomId -> Word32 -> Word32 -> Bool -> IO ()
setIOPortPermission dom first num allow =
  domainControlOp DomCtlSetIOPortPerm (fromDomId dom)
    (buildIOPortPermReq first num allow)
    (\ _ _ _ -> return ())

-- |Set whether or not the given domain is allowed to access the given IO
-- memory. Note that the second argument is the physical page number of the
-- memory (not the virtual address), and the third argument is how many pages
-- after that (inclusive) the domain should or should not be able to access.
setIOMemoryPermission :: DomId -> MFN -> Word64 -> Bool -> IO ()
setIOMemoryPermission dom mfn num allow  =
  domainControlOp DomCtlSetIOMemoryPerm (fromDomId dom)
    (buildIOMemPermReq (fromIntegral (fromMFN mfn)) num allow)
    (\ _ _ _ -> return ())

-- |Set the IO privilege level (IOPL) for the current VCPU.
setIOPrivilegeLevel :: Word32 -> IO ()
setIOPrivilegeLevel l =
  physicalDeviceOp PhysDevOpSetIOPrivLevel (\ p -> poke p l) (\ _ _ ->return ())

-- |Send an end of interrupt (EOI) message for the given IRQ
sendEOI :: Word32 -> IO ()
sendEOI irq =
  physicalDeviceOp PhysDevOpEOI (\ p -> poke p irq) (\ _ _ -> return ())

-- |Get basic information about a group of domains. The first argument is the
-- first domain you want information about, and the second is the number of
-- domains after that you want to get information about. Keep in mind that
-- the relevant data structures are fairly large, so asking for information
-- on a large number of domains is going to have serious memory use
-- repurcussions.
domainList :: DomId -> Word32 -> IO [DomainInfo]
domainList dom num =
  systemControlOp SysCtlGetDomInfoList
    (buildGetDomInfoListReq (fromDomId dom) num)
    readGetDomInfoListResp

