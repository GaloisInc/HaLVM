-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com> and Adam Wick <awick@galois.com>
-- BANNEREND
-- |A library to hide all the gory details of establishing peer-to-peer IVC
-- communication patterns using "Hypervisor.IVC".
--
-- The current implementation uses the XenStore. Therefore, using this
-- library requires programs to initialize halvm_kernel or
-- halvm_kernel_daemon with the 'XenDevice.Xenbus.dXenbus' device.
-- Also, the XenStore must have been initialized with the
-- make_halvm_dir.py script included with HALVM distributions.

module RendezvousLib.PeerToPeer(
  P2PConnection(..),
  ) where

import Control.Concurrent
import Hypervisor.Basics
import Hypervisor.Debug
import Hypervisor.Port

import Communication.IVC(InChannelEx, OutChannelEx, InOutChannelEx, Channel(..),
                         mkInChannelName, mkOutChannelName, mkInOutChannelName,
                         acceptChannel, makeChannelTo, PageReference,
                         AcceptChannel, MakeChannelTo)

import System.Exit 
import XenDevice.Xenbus

-- |Start a peer to peer rendezvous. Each peer-to-peer rendezvous must
-- be started by one peer with this function, and finished by the other
-- peer using the other function. The key passed to both functions must
-- be identical and globally unique to this connection. The handler function
-- is called when the other domain is detected, and should simply convert
-- the event channel and grant reference into whatever higher-level IVC
-- structure is required.
--
-- The most common cause of failure at this point will be a nonexistent
-- halvm directory in the Xenstore.
offerPToPConnection :: String -> 
                       (DomId -> [PageReference] -> Port -> IO a) ->
                       IO (Maybe a)
offerPToPConnection key builder =
    do check <- xsDirectory "/halvm"
       case check of
         XBOk _ ->
             do let base_dir = "/halvm/" ++ key
                myId <- myDomId
                XBOk _ <- xsRm base_dir
                XBOk _ <- xsWrite (base_dir ++ "/starterDomId") (show myId)
                -- TODO: check that noone else writes starterDomId
                oIdStr <- forceRead $ base_dir ++ "/accepterDomId"
                oGRefStr <- forceRead $ base_dir ++ "/grant-refs"
                oEChanStr <- forceRead $ base_dir ++ "/event-channel"
                res <- builder (read oIdStr) (read oGRefStr) (read oEChanStr)
                XBOk _ <- xsRm base_dir
                return $ Just res
         _ ->
             return Nothing

-- |Accept a peer to peer connection to someone else; this is the dual of
-- offerPToPConnection, and so the keys used must match exactly. The function
-- argument is used to generate the grant reference and event channel
-- information to be passed to the originating domain. The final value in 
-- that tuple is returned from acceptPToPConnection, and should most likely
-- be the higher-level IVC mechanism being used.
acceptPToPConnection :: String -> 
                        (DomId -> IO (Maybe ([PageReference], Port, a))) ->
                        IO (Maybe a)
acceptPToPConnection key builder =
    do check <- xsDirectory "/halvm"
       case check of
         XBOk _ ->
             do let base_dir = "/halvm/" ++ key ++ "/"
                oIdStr <- forceRead $ base_dir ++ "starterDomId"
                let oid = read oIdStr
                components <- builder oid
                case components of
                  Just (grefs, echan, res) ->
                      do myId <- myDomId
                         XBOk _ <- xsWrite (base_dir++"accepterDomId") 
				           (show myId)
                         XBOk _ <- xsWrite (base_dir ++ "grant-refs")
				           (show grefs)
                         XBOk _ <- xsWrite (base_dir ++ "event-channel")
					   (show echan)
                         return $ Just $ res
                  _ -> return Nothing
         _ -> return Nothing

forceRead :: String -> IO String
forceRead path =
    do r <- xsRead path
       case r of
         XBOk s -> return s
         XBError "ENOENT" -> do threadDelay 1000000 -- 1 sec
                                forceRead path
         XBError err -> 
             do writeDebugConsole $ "Force read failed:" ++ path ++ "\n"
                writeDebugConsole $ "  error = " ++ err ++ "\n"
                System.Exit.exitFailure

-------------------------------------------------------
-- New interface


class P2PConnection c1 c2 | c1 -> c2, c2 -> c1 where

  -- | Given a key that is unique throughout the virtual platform,
  -- provide the two ends of a typed channel for peer-to-peer
  -- communication between two domains.  The first component is
  -- offering the communication, the second component is accepting the
  -- communication.
  p2pConnection :: String -> (IO c1, IO c2)
  p2pConnection = sizedP2PConnection 1

  -- | Just like p2pConnection, but you can provide the number of pages
  -- to use. p2pConnection is equivalent to sizedP2PConnection 1.
  sizedP2PConnection :: Int -> String -> (IO c1, IO c2)

instance P2PConnection (InChannelEx t a) (OutChannelEx t a) where
  sizedP2PConnection x _ | x < 1 = error "Must have at least one page!"
  sizedP2PConnection n s = (offerPToPConnection' mkInChannelName s n,
                            acceptPToPConnection' s n)

instance P2PConnection (OutChannelEx t a) (InChannelEx t a) where
  sizedP2PConnection x _ | x < 1 = error "Must have at least one page!"
  sizedP2PConnection n s = (offerPToPConnection' mkOutChannelName s n,
                            acceptPToPConnection' s n)

instance P2PConnection (InOutChannelEx t i o) (InOutChannelEx t o i) where
  sizedP2PConnection x _ | x < 1 = error "Must have at least one page!"
  sizedP2PConnection n s = (offerPToPConnection' mkInOutChannelName s n,
                            acceptPToPConnection' s n)

offerPToPConnection' :: AcceptChannel cn a =>
                        (DomId -> [PageReference] -> Port -> cn) ->
                        String -> Int ->
                        IO a
offerPToPConnection' cc key _size =
  do a <- offerPToPConnection key $ \d g p -> acceptChannel (cc d g p)
     case a of
       Nothing -> fail "No /halvm directory in XenStore"
       Just c -> return c

acceptPToPConnection' :: (Channel a, MakeChannelTo a) =>
                         String -> Int ->
                         IO a
acceptPToPConnection' key size =
  do a <- acceptPToPConnection key $ \d -> 
       do c <- makeChannelTo d (fromIntegral size)
          return (Just (pageReference c, port c, c))
     case a of
       Nothing -> fail "No /halvm directory in XenStore."
       Just c -> return c
