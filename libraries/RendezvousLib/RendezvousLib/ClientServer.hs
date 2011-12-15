-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com> and Adam Wick <awick@galois.com>
-- BANNEREND
-- |A library to hide all the gory details of establishing
-- client-server communication patterns using "Hypervisor.IVC".
--
-- The current implementation uses the XenStore. Therefore, using this
-- library requires programs to initialize halvm_kernel or
-- halvm_kernel_daemon with the 'XenDevice.Xenbus.dXenbus' device.
-- Also, the XenStore must have been initialized with the
-- make_halvm_dir.py script included with HALVM distributions.
module RendezvousLib.ClientServer(
  Listener, accept,
  ClientServerConnection(..),
  clientServerRingBufferConnections,
  ) where

import Data.List
import Hypervisor.Basics
import Hypervisor.Port
import Communication.IVC(makeChannelTo,Channel(pageReference,port),MakeChannelTo
                        ,acceptChannel, InChannelEx, OutChannelEx, AcceptChannel
                        ,InOutChannelEx, mkInChannelName, mkOutChannelName
                        ,mkInOutChannelName, PageReference(..))
import XenDevice.Xenbus
import Communication.RingBuffer(FrontRingBuffer, FrontRingBufferable
                               ,BackRingBuffer,  BackRingBufferable
                               ,brbAttach, frbCreate)
import Control.Concurrent.MVar(newEmptyMVar, takeMVar, putMVar)
import Control.Monad(unless)
import Control.Monad.Fix(MonadFix(mfix))

{-# DEPRECATED startServerListener, rendezvousWithServer
    "Please upgrade to the newer interface in RendezvousLib.ClientServer." #-}

-- |Start the server side of a client-server XenStore rendezvous,
-- using the given key. The handler is run upon every new connection
-- to the System.  The arguments to the handler are a list of grant
-- references, a list of event channels, and a general info directory
-- for passing any other information. Lists are used so that domains
-- that want to set up multiple IVC channels at the same time can do
-- so. 
--
-- Probably the most common reason for this to fail is that the
-- halvm directory doesn't exist in the XenStore. Simply run the
-- make_halvm_dir.py script included in the HALVM directory to fix
-- this.
startServerListener :: String -> 
                       (DomId -> [[PageReference]] -> 
                                 [Port] -> 
                                 [(String, String)] -> 
                                 IO ()) 
                       -> 
                       IO Bool
startServerListener key handler =
    do check <- xsDirectory "/halvm"
       case check of
         XBOk _ ->
             do let path = "/halvm/" ++ key 
                myId <- myDomId
                xsRm path
                XBOk _ <- xsMkDir path
                XBOk _ <- xsWrite (path ++ "/server-id") (show myId)
                XBOk _ <- xsMkDir (path ++ "/clients")
                XBOk _ <- xsSetWatch (path ++ "/clients") xenstoreCallback
                return True
         _ ->
             return False
    where xenstoreCallback :: WatchId -> String -> IO ()
          xenstoreCallback _ path =
              if "/state" `isSuffixOf` path
                 then do let prefix = take (length path - length "/state") path
                         _domIdStr <- xsRead $ prefix ++ "/domain-id"
                         _infoStr <- xsRead $ prefix ++ "/info"
                         refs <- readMult 0 (prefix ++ "/grant-refs")
                         echans <- readMult 0 (prefix ++ "/event-channel")
                         case (_domIdStr, _infoStr) of
                           (XBOk domIdStr, XBOk infoStr) ->
                               handler (read domIdStr) refs echans (read infoStr)
                           _ ->
                               return ()
                 else return ()
          readMult :: (Read a) => Int -> String -> IO [a]
          readMult num prefix =
              do val <- xsRead $ prefix ++ (show num)
                 case val of
                   XBOk strVal ->
                       do rest <- readMult (num + 1) prefix
                          return ((read strVal):rest)
                   XBError _ ->
                       return []

-- |Try to connect and start a connection to an existing server using the 
-- given key, with any additional metadata in the given dictionary. The
-- final argument is a constructor for building whatever grant references
-- and event channels the domain wants communicated to the server. The
-- final part of the return tuple is returned with the underlying function,
-- and should probably be whatever higher-level IVC channel this rendezvous
-- is setting up; a Hypervisor.IVC channel, a ring buffer, etc..
rendezvousWithServer :: String -> [(String,String)] ->
                        (DomId -> IO ([([PageReference],Port)],a)) ->
                        IO (Maybe a)
rendezvousWithServer key info builder =
    do check <- xsDirectory "/halvm"
       case check of
         XBOk _ ->
             do fed <- xsDirectory $ "/halvm/" ++ key ++ "/clients"
                beId <- xsRead $ "/halvm/" ++ key ++ "/server-id"
                case (fed, beId) of
                  (XBOk _, XBOk serverIdStr) ->
                      do let serverId = read serverIdStr
                         myId@(DomId numId) <- myDomId
                         let prefix = "/halvm/" ++ key ++ "/clients/" ++ 
                                      (show numId)
                         (grantschans,res) <- builder serverId
                         let (grants,chans) = unzip grantschans
                         xsMkDir prefix
                         xsWrite (prefix ++ "/domain-id") (show myId)
                         xsWrite (prefix ++ "/info") (show info)
                         writeMult 0 (prefix ++ "/grant-refs") grants
                         writeMult 0 (prefix ++ "/event-channel") chans
                         xsWrite (prefix ++ "/state") "Sort_of_goodish"
                         return $ Just res
                  _ ->
                      return Nothing
         _ ->
             return Nothing
    where writeMult :: (Show a) => Int -> String -> [a] -> IO ()
          writeMult _ _ [] = return ()
          writeMult num prefix (first:rest) =
              do xsWrite (prefix ++ (show num)) (show first)
                 writeMult (num + 1) prefix rest

rendezvousWithServer2 :: String -> [(String, String)] ->
                         (DomId -> IO ([([PageReference], Port)], a)) ->
                         IO a
rendezvousWithServer2 key info builder = 
  do res <- rendezvousWithServer key info builder
     case res of
        Just a -> return a
        Nothing -> fail "Cannot connect with server."


-------------------------------------------------------

-- | Type for server listeners.  A listener enables a server to accept
-- any number of connections from clients.
data Listener c = Listener { lAccept :: IO c
                           -- ...
                           }

-- | Accept a new connection from a client.
accept :: Listener c -> IO c
accept = lAccept

class ClientServerConnection c s | c -> s, s -> c where

  -- | Given a key that is unique throughout the virtual platform,
  -- provide a pair of operations for client-server communication.
  -- The server component is offering communication through a
  -- listener, and any number of virtual machines may use the client
  -- component to connect to the server, given that the server will
  -- accept them.
  clientServerConnection :: String -> (IO c, IO s)
  clientServerConnection = sizedClientServerConnection 1

  -- | As above, but use the given number of pages to build the 
  -- channel.
  sizedClientServerConnection :: Int -> String -> (IO c, IO s)


instance ClientServerConnection (InChannelEx t a) (Listener (OutChannelEx t a)) where
  sizedClientServerConnection x _ | x < 1 = error "Must have at least one page!"
  sizedClientServerConnection n s = 
    (ivcclient s n, ivcserver mkOutChannelName s n)

instance ClientServerConnection (OutChannelEx t a) (Listener (InChannelEx t a)) where
  sizedClientServerConnection x _ | x < 1 = error "Must have at least one page!"
  sizedClientServerConnection n s = 
    (ivcclient s n, ivcserver mkInChannelName s n)

instance 
  ClientServerConnection (InOutChannelEx t i o) (Listener (InOutChannelEx t o i)) where
  sizedClientServerConnection x _ | x < 1 = error "Must have at least one page!"
  sizedClientServerConnection n s = 
    (ivcclient s n, ivcserver mkInOutChannelName s n)

gp :: Channel c => c -> ([PageReference], Port)
gp c = (pageReference c, port c)

ivcclient :: (Channel a, MakeChannelTo a) => String -> Int -> IO a
ivcclient s n = rendezvousWithServer2 s [] $ \d ->
                  do c1 <- makeChannelTo d (fromIntegral n)
                     return ([gp c1],c1)

ivcserver :: AcceptChannel cn a =>
             (DomId -> [PageReference] -> Port -> cn) ->
             String -> Int ->
             IO (Listener a)
ivcserver cc s _n = listener s $ \ d [(g,p)] -> acceptChannel (cc d g p)

-- | Given a unique key, provide a pair of operations for establishing
-- ring-buffer communication between clients and a server.  The server
-- operation returns a listener, which can be used for accepting
-- client connections.

clientServerRingBufferConnections
  :: (FrontRingBufferable a b c, BackRingBufferable a b c)
  => String -> (IO (FrontRingBuffer a b c), (BackRingBuffer a b c -> a -> IO b)
  -> IO (Listener (BackRingBuffer a b c)))
clientServerRingBufferConnections s = (ringclient s, ringlistener s)

listener :: String -> (DomId -> [([PageReference], Port)] -> IO a)
         -> IO (Listener a)
listener s f = do
  v <- newEmptyMVar
  b <- startServerListener s $ \d gs ps _ ->
         f d (zip gs ps) >>= putMVar v
          -- TODO: this attaches too early, we should really 
          -- wait for the accept call.
  unless b $ fail "Failed to set up server.  Check that /halvm exists in XenStore."
  return Listener{ lAccept = takeMVar v }

ringlistener :: BackRingBufferable rqt rpt idt
             => String -> (BackRingBuffer rqt rpt idt -> rqt -> IO rpt)
             -> IO (Listener (BackRingBuffer rqt rpt idt))
ringlistener s bf = listener s
                  $ \ d [([PR_GrantRef g],p)] -> mfix
                  $ \ mb -> brbAttach d g p (bf mb)
                            
ringclient :: FrontRingBufferable rqt rpt idt
           => String -> IO (FrontRingBuffer rqt rpt idt)
ringclient s = rendezvousWithServer2 s [] $ \ d ->
            do (frb, gref, p) <- frbCreate d 
               return ([([PR_GrantRef gref],p)],frb)
