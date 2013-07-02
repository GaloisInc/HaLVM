-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Various pieces of useful information about the current domain.
module Hypervisor.DomainInfo(
         DomId
       , DomainHandle
       , newDomainHandle
       , SID
#ifdef TESTING
       , toSID
#endif
       , VCPU
       , fromDomId, toDomId
       , fromVCPU, toVCPU
       , DomainFlags(..)
       , domidSelf
       , xenMagicString
       , domainFlags
       , domainModuleStart
       , domainModuleLength
       , DomainInfo(..)
       , DomainInfoFlag(..)
       , domainInfo
       , domainProcessorContext
       , setDomainProcessorContext
       )
 where

import Data.Bits
import Data.Data
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Hypercalls.DomainControl
import {-# SOURCE #-} Hypervisor.Memory(PFN,toPFN,VPtr)
import Hypervisor.Structures.DomainInfo
import Hypervisor.Structures.VCPUContext
import Text.Printf

-- |A domain identifier
newtype DomId = DomId Word32
  deriving (Eq, Ord, Typeable, Data)

instance Show DomId where
  show (DomId x) = "dom" ++ show x

-- |A domain handle
newtype DomainHandle = DomHandle [Word8]
  deriving (Eq, Ord, Typeable, Data)

instance Show DomainHandle where
  show (DomHandle xs) = concatMap (printf "%02x") xs

newDomainHandle :: [Word8] -> DomainHandle
newDomainHandle xs = DomHandle (xs' ++ replicate (16 - length xs') 0)
 where xs' = take 16 xs

instance Storable DomainHandle where
  sizeOf _             = 16
  alignment _          = 1
  peek p               = DomHandle `fmap` peekArray 16 (castPtr p)
  poke p (DomHandle h) = pokeArray (castPtr p) h

-- |A security identifier
newtype SID = SID Word32
  deriving (Eq, Ord, Typeable, Data, Show, Storable)

#ifdef TESTING
toSID :: Word32 -> SID
toSID x = SID x
#endif

-- |A VCPU identifier
newtype VCPU = VCPU Word
 deriving (Eq,Ord,Typeable,Data)

instance Show VCPU where
  show (VCPU x) = "vcpu:" ++ show x

-- |Translate a VCPU to a number
fromVCPU :: Integral a => VCPU -> a
fromVCPU (VCPU x) = fromIntegral x

-- |Translate a number fo a VCPU
toVCPU :: Integral a => a -> VCPU
toVCPU x = VCPU (fromIntegral x)

-- |Translate from a DomId
fromDomId :: Integral a => DomId -> a
fromDomId (DomId x) = fromIntegral x

-- |Translate to a DomId
toDomId :: Integral a => a -> DomId
toDomId x = DomId (fromIntegral x)

-- |The possible flags we might be passed at startup.
data DomainFlags = DomainPrivileged
                 | DomainInitial
                 | DomainModuleIsMultiboot
                 | DomainModuleIsAFrame
  deriving (Eq,Show)

-- |This is a generic domain identifier that can be use to refer to oneself
-- without knowing your true domain id. It is only really useful in a limited
-- subset of direct Xen upcalls; you really should not use this when trying
-- to collaborate with other virtual machines.
domidSelf :: DomId
domidSelf  = DomId 0x7FF0

-- |Provide the Xen magic string read on boot from the system.
xenMagicString :: IO String
xenMagicString  = do
  ptr <- getMagicStringPtr
  peekCAStringLen (ptr, 32)

-- |Provide a list of flags passed to this domain.
domainFlags :: IO [DomainFlags]
domainFlags = do
  flagVal <- getDomainFlags
  return $ (if testBit flagVal 0 then [DomainPrivileged]        else [])
        ++ (if testBit flagVal 1 then [DomainInitial]           else [])
        ++ (if testBit flagVal 2 then [DomainModuleIsMultiboot] else [])
        ++ (if testBit flagVal 3 then [DomainModuleIsAFrame]    else [])

-- |Provide the start location for any accompanying modules.
domainModuleStart :: IO (Either (VPtr Word8) PFN)
domainModuleStart = do
  startVal <- getDomainModuleStart
  dflags   <- domainFlags
  case startVal of
    0                                      -> return (Left nullPtr)
    x | DomainModuleIsAFrame `elem` dflags -> return (Right (toPFN startVal))
      | otherwise                          -> return (Left ptrVal)
       where ptrVal = nullPtr `plusPtr` (fromIntegral x)

-- |Get basic information about the given domain. This routine either requires
-- the domain to be privileged, or for a custom XSM policy to be installed
-- allowing it.
domainInfo :: DomId -> IO DomainInfo
domainInfo (DomId targ) =
  domainControlOp DomCtlGetDomainInfo (fromIntegral targ)
                  (\ _ -> return ())
                  (\ _ _ resp -> peek resp)

-- |Fetch the entire processor context for the given VCPU of the given domain.
domainProcessorContext :: DomId -> VCPU -> IO ProcessorContext
domainProcessorContext (DomId did) (VCPU v) =
  domainControlOp DomCtlGetContext (fromIntegral did)
                  (buildVCPUContextRequest (fromIntegral v) Nothing)
                  (\ _ ctxtp _ -> peek ctxtp)

-- |Set the processor context for the given VCPU of the given domain.
setDomainProcessorContext :: DomId -> VCPU -> ProcessorContext -> IO ()
setDomainProcessorContext (DomId did) (VCPU v) ctxt =
  domainControlOp DomCtlSetContext (fromIntegral did)
                  (buildVCPUContextRequest (fromIntegral v) (Just ctxt))
                  (\ _ _ _ -> return ())

foreign import ccall unsafe "domain_info.h get_magic_string_ptr"
  getMagicStringPtr :: IO (Ptr CChar)

foreign import ccall unsafe "domain_info.h get_domain_flags"
  getDomainFlags :: IO Word32

foreign import ccall unsafe "domain_info.h get_domain_mod_start"
  getDomainModuleStart :: IO Word

-- |Return the length of the module, in bytes.
foreign import ccall unsafe "domain_info.h get_domain_mod_len"
  domainModuleLength :: IO Word
