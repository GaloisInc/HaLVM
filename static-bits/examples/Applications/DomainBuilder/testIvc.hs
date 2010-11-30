{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- This is experimental throw-away code for the purpose of experimenting
-- with static-channel setup between child domains.
import Control.Concurrent(threadDelay)

import DomainBuilder.Build
import DomainBuilder.IfaceTypes(Access(Access_readWrite))
import DomainBuilder.ModuleInfo(getModule)

import Hypervisor.Basics(DomId, SID(..))
import Hypervisor.Debug(writeDebugConsole)
import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Port(allocUnboundPort, Port)
import Hypervisor.Privileged(unpauseDomain)
import Communication.IVC(mkInChannelName, mkOutChannelName, PageReference(..), 
                     InChannelName, OutChannelName, marshall)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Generics(Data)

main :: IO ()
main = do halvm_kernel [] $ const $ do
            go
            writeDebugConsole "testIvc ended now\n"
            threadDelay 5000000

-- This stuff needs to go into IVC eventually, in some form.

class ChannelNames c1 c2 | c1 -> c2, c2 -> c1 where
  channelNames :: DomId -> PageReference -> Port -> (c1,c2)

instance ChannelNames (InChannelName a) (OutChannelName a) where
  channelNames d r p = (mkInChannelName d [r] p, mkOutChannelName d [r] p)


setCL :: Data a => DomainHandle -> a -> IO ()
setCL k a = setCommandLine k (BS.unpack (marshall a))

receiverSID :: SID
receiverSID = SID 3

senderSID :: SID
senderSID = SID 4

go :: IO ()
go
 = do (elf,_) <- getModule 0
      (receiver,mem_map) <- buildDomain' receiverSID
                             (64 * 1024) elf Nothing [Access_readWrite]
      writeDebugConsole (show mem_map ++ "\n")
      let (_,[mfn1]) = blank_pages mem_map
      sender <- buildDomain senderSID (64 * 1024) elf ""
      Right port1 <- allocUnboundPort (domainId receiver) (domainId sender)
      let (icn,ocn) = channelNames (domainId receiver) (PR_MFN mfn1) port1
      setCL receiver (Left icn::Either (InChannelName Int) (OutChannelName Int))
      setCL sender (Right ocn ::Either (InChannelName Int) (OutChannelName Int))
      unpauseDomain (domainId receiver)
      unpauseDomain (domainId sender)
      return ()

