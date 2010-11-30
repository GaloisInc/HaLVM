{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

--
-- ChatDemo: An implementation of a multi-person chat room for HALVM
--
-- This file contains the code for both sides (client and server) for the 
-- connections.
--
-- Both sides are multithreaded. The client (the simpler case), uses one
-- thread to get input from the user, and the other to print out 
-- messages received through the server.
--
-- The server's main thread is used to aggregate the messages sent in from
-- the clients, determine who can see what, and send those messages out to
-- the clients. The server uses an auxiliary thread to listen for new 
-- connections in the Xen store, and this thread in turn spawns one listener 
-- thread per client connection.
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.

-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--

import Control.Concurrent
import qualified Hypervisor.Debug
import Hypervisor.Kernel
import RendezvousLib.ClientServer(ClientServerConnection(..), Listener, accept)
import qualified System.Exit
import XenDevice.Console
import qualified XenDevice.Xenbus as XB

import Communication.IVC(InChannelName, OutChannelName, peer,
                         OutChannel, InChannel, makeChannelTo,
                         acceptChannel, channelName,
                         PutChannel(..), GetChannel(..))

-------------------------------------------------------------------------------
-- A very simple security model

data SecurityLevel =
     Unclassified
   | Secret
   | TopSecret
   deriving (Show,Read,Eq,Ord)

type Person = String

-- A clearance associates a person with a security level
type Clearance = (Person,SecurityLevel)

-- A connected person has the person's name and the function to write output
-- to them
type ConnectedPerson = (Person, Writer)

clearances :: [Clearance]
clearances = [("Adam", Unclassified),
	      ("Andrew", TopSecret),
	      ("Andy", TopSecret),
	      ("Dylan", Secret),   
	      ("Lee", Unclassified),
	      ("Zebra", Secret)]

-- Determine the clearance of a person (Unclassified by default)  
clearanceOf :: Person -> SecurityLevel
clearanceOf p = 
  case lookup p clearances of
    Just c -> c
    Nothing -> Unclassified

-- The key judgement: may person A read data from person B?
mayReadFrom :: Person -> Person -> Bool
a `mayReadFrom` b = clearanceOf a >= clearanceOf b


----------------------------------------------------------------------------
-- Connection patterns
--
-- Each client connects to the server and sends one introductory message 
-- with its name, and a pair of chat channels
-- From then on, the introduction channel is not used anymore.
-- Instead, the actual communication takes place on the pair of
-- chat channels.

type Chat = String
type Introduction = (Person, InChannelName Chat, OutChannelName Chat)

connector :: IO (OutChannel Introduction)
acceptor :: IO (Listener (InChannel Introduction))
(connector,acceptor) = clientServerConnection "ChatDemo"

-------------------------------------------------------------------------------

-- Definition of the client

type Reader = IO Chat
type Writer = Chat -> IO ()

client :: IO ()
client = do 
   s <- connector
   name <- getName
   oc <- makeChannelTo (peer s) (fromIntegral (1::Int))
   ic <- makeChannelTo (peer s) (fromIntegral (1::Int))
   icn <- channelName oc
   ocn <- channelName ic
   put s (name, icn, ocn)
   forkIO $ client_getMessage (get ic)
   client_getInput (put oc)

client_getMessage :: Reader -> IO ()
client_getMessage serverReader = 
  repeatM_ $ do
    s <- serverReader 
    writeConsole s

client_getInput :: Writer -> IO ()
client_getInput serverWriter =
  repeatM_ $ do
    s <- echoReader (readConsole 1)
    serverWriter s

-------------------------------------------------------------------------------
-- Definition of the server

-- This data type is used within the server to communicate from
-- message and connection handling threads to the main thread
data ServerRequest = MessageFrom Person String
                   | Join Person Writer

server :: IO ()
server = do 
  a <- acceptor
  reqChan <- newChan 
  forkIO $ server_processRequests reqChan []
  repeatM_ $ do c <- accept a
                server_startClientConnection reqChan c

server_processRequests :: Chan ServerRequest -> [ConnectedPerson] -> IO ()
server_processRequests reqChan persons =
  do msg <- readChan reqChan
     case msg of
       MessageFrom sender message -> 
         do let message' = "[" ++ sender ++ ":" ++ (show (clearanceOf sender)) ++ "] " ++ message
            writeConsole message'
	    let targets = filter (sender `shouldCopyTo`) persons
	    mapM_ (copy message') targets
            server_processRequests reqChan persons
       Join name writer -> 
         do writer "Connected.\n"
            server_processRequests reqChan ((name,writer):persons)
  where
    sender `shouldCopyTo` (name,_) = name `mayReadFrom` sender
    copy s (_,writer) = writer s

server_manageClientConnection :: Person -> Reader -> Chan ServerRequest -> IO ()
server_manageClientConnection name reader reqChan =
  repeatM_ $ do
    line <- getLineR reader
    writeChan reqChan $ MessageFrom name line

server_startClientConnection :: Chan ServerRequest -> InChannel Introduction
                                -> IO ()
server_startClientConnection reqChan introChan = 
  do (name, icn, ocn) <- get introChan
     ic <- acceptChannel icn
     oc <- acceptChannel ocn
     writeChan reqChan $ Join name (put oc)
     forkIO $ server_manageClientConnection name (get ic) reqChan
     writeConsole $ "Found new client: " ++ name ++ "\n"

-------------------------------------------------------------------------------

-- Xenstore stuff

getName :: IO String
getName = do
  r <- XB.xsRead "name"
  case r of
    XB.XBOk domid  -> return domid
    XB.XBError err -> cdFail $ "getName error:" ++ err

-------------------------------------------------------------------------------

-- The main dispatch function

main :: IO ()
main = halvm_kernel_daemon [dConsole, XB.dXenbus] main'

main' :: [String] -> IO ()
main' args = 
    case args of
      ["mode=server"] -> server
      ["mode=client"] -> client
      _ -> cdFail $ "Unkown mode for ChatDemo: " ++ show args


-------------------------------------------------------------------------------

-- Utilities


cdFail :: forall a . String -> IO a
cdFail str = do hDBC $ str ++ "\n"
                System.Exit.exitFailure

hDBC :: String -> IO ()
hDBC x = Hypervisor.Debug.writeDebugConsole x

--  Repeat a monadic action indefinitely 
repeatM_ :: Monad m => m a -> m ()
repeatM_ m = sequence_ $ repeat m


echoReader :: Reader -> Reader
echoReader reader = do 
  s <- reader
  writeConsole s
  return s

getLineR :: Reader -> IO String
getLineR reader = do
  [c] <- reader
  if c == '\n' 
    then 
      return ['\n'] 
    else do
      cs <- getLineR reader
      return $ c:cs
