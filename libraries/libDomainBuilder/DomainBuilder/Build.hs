-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>, Iavor S. Diatchki <diatchki@galois.com>
-- BANNEREND
module DomainBuilder.Build
  ( buildDomain
  , buildDomain'
  , setCommandLine
  , setStoreMfn
  , setStoreEvtChn
  , setConsoleDomUMfn
  , setConsoleDomUEvtChn
  , Cv.DomainHandle
  , Cv.domainId
  , Cv.MemMap(..)
  , Cv.withStartPage
  , Access(..)
  ) where

import qualified DomainBuilder.CreateVM as Cv
import DomainBuilder.IfaceTypes(Access(..))
import qualified DomainBuilder.StartPage as StartPage
import qualified DomainBuilder.SafePage as Sp

import Hypervisor.Port(Port)
import Hypervisor.Memory(MFN)
import Hypervisor.Basics(SID)

import qualified Data.ByteString as BS
import Data.Word(Word64)
import Control.Monad(when)
import Foreign.Storable(Storable)

setCommandLine :: Cv.DomainHandle -> String -> IO ()
setCommandLine h a' =
  do let a = a' ++ "\000"
     when (length a > fromIntegral StartPage.max_guest_cmdline)
          (fail "Error_internalSetElemFailed")
     rs <- Cv.withStartPage h $ \sp ->
       mapM (\ (c,o) -> Sp.setElem sp (StartPage.cmd_line+o) c) (zip a [0..])
     when (not (and rs))
          (fail "Failed to set command line")

-- |Set the XenStore frame number for the given domain to the given
-- *machine* frame.
setStoreMfn :: Cv.DomainHandle -> MFN -> IO ()
setStoreMfn h m = setStartPage h StartPage.store_mfn m

-- |Set the event channel number for the given domain to the given port.
setStoreEvtChn :: Cv.DomainHandle -> Port -> IO ()
setStoreEvtChn h p = setStartPage h StartPage.store_evtchn p

-- |Set the console frame number for the given domain to the given
-- *machine* frame.
setConsoleDomUMfn :: Cv.DomainHandle -> MFN -> IO ()
setConsoleDomUMfn h m = setStartPage h StartPage.console_domU_mfn m

-- |Set the console event channel for the given domain to the given
-- port.
setConsoleDomUEvtChn :: Cv.DomainHandle -> Port -> IO ()
setConsoleDomUEvtChn h p = setStartPage h StartPage.console_domU_evtchn p

setStartPage :: Storable a => Cv.DomainHandle -> Sp.Offset -> a -> IO ()
setStartPage h o v =
  do ok <- Cv.withStartPage h $ \sp -> Sp.setElem sp o v
     when (not ok) $ fail $ "setStartPage: failed to set @ " ++ show o

-- |Create a new domain. The arguments are, in order, the security ID for the
-- domain to be created, the maximum amount of memory in kilobytes, a byte
-- string representing the static image for the new domain, and the command
-- line to pass it. This routine will not start the domain; you will have to
-- start it yourself using the unpauseDomain function.
buildDomain :: SID -> Word64 -> BS.ByteString -> String -> IO Cv.DomainHandle
buildDomain sid maxMemory image cmdline =
  do (h, _) <- buildDomain' sid maxMemory image Nothing []
     setCommandLine h cmdline
     return h

-- |A more generalized version of buildDomain. Again, the function takes the
-- security ID for the domain to be created, the max memory in kilobytes and
-- the static image. In addition, this routine potentially takes an initrd
-- (or similar) image and a list that is used to allocate some extra blank
-- pages.
buildDomain' :: SID -> Word64 -> BS.ByteString -> Maybe BS.ByteString
             -> [Access] -> IO (Cv.DomainHandle,Cv.MemMap)
buildDomain' = Cv.createVm Nothing



