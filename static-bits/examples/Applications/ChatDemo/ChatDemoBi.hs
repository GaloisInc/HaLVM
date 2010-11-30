{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
--
-- ChatDemoBi: A variant of the chat demo that uses bidirectional IVC channels.
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
import RendezvousLib.ClientServer(clientServerConnection, accept, Listener)
import Data.Generics.Basics(Data)
import qualified System.Exit
import XenDevice.Console
import qualified XenDevice.Xenbus as XB

import Communication.IVC(get, put, InOutChannel, GetChannel)

---------------------------------------------------------------------
-- A very simple security model

data SecurityLevel =
     Unclassified
   | Secret
   | TopSecret
   deriving (Show,Read,Eq,Ord)

type Person = String

-- A clearance associates a person with a security level
type Clearance = (Person,SecurityLevel)

-- A connected person has the person's name and the server-end channel.
type ConnectedPerson = (Person, ServerChannel)

clearances :: [Clearance]
clearances = [("Adam",TopSecret),
	      ("Andrew", TopSecret),
	      ("Andy", Unclassified),
	      ("Dylan", Secret),   
	      ("Lee", Unclassified)]

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
-- with its name.  After that, there is only Chat messages going
-- between the client and the server.

type Chat = String
type ClientChannel = InOutChannel Chat (Either Person Chat)
type ServerChannel = InOutChannel (Either Person Chat) Chat
connector :: IO ClientChannel
acceptor :: IO (Listener ServerChannel)
(connector,acceptor) = clientServerConnection "ChatDemo"

----------------------------------------------------------------------------

-- Definition of the client

type Reader = IO Chat

client :: IO ()
client = do 
   s <- connector
   name <- getName
   put s (Left name)
   forkIO $ client_getMessage s
   client_getInput s

client_getMessage :: ClientChannel -> IO ()
client_getMessage cc = 
  repeatM_ $ do
    s <- get cc
    writeConsole s

client_getInput :: ClientChannel -> IO ()
client_getInput cc =
  repeatM_ $ do
    s <- echoReader (readConsole 1)
    put cc (Right s)

-------------------------------------------------------------------------------
-- Definition of the server

-- This data type is used within the server to communicate from
-- message and connection handling threads to the main thread
data ServerRequest = MessageFrom Person String
                   | Join Person ServerChannel

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
       Join name sc -> 
         do put sc "Connected.\n"
            server_processRequests reqChan ((name,sc):persons)
  where
    sender `shouldCopyTo` (name,_) = name `mayReadFrom` sender
    copy s (_,sc) = put sc s

server_manageClientConnection :: 
  Person -> ServerChannel -> Chan ServerRequest -> IO ()
server_manageClientConnection name sc reqChan =
  repeatM_ $ do
    line <- getLineR (getRight sc)
    writeChan reqChan $ MessageFrom name line

server_startClientConnection :: Chan ServerRequest -> ServerChannel -> IO ()
server_startClientConnection reqChan sc =
  do name <- getLeft sc
     writeChan reqChan $ Join name sc
     forkIO $ server_manageClientConnection name sc reqChan
     writeConsole $ "Found new client: " ++ name ++ "\n"

----------------------------------------------------------------------------------

-- Xenstore stuff

getName :: IO String
getName = do
  r <- XB.xsRead "name"
  case r of
    XB.XBOk domid  -> return domid
    XB.XBError err -> cdFail $ "getName error:" ++ err

-----------------------------------------------------------------------------------------------

-- The main dispatch function

main :: IO ()
main = halvm_kernel [dConsole, XB.dXenbus] main'

main' :: [String] -> IO ()
main' args = 
    case args of
      ["mode=server"] -> server
      ["mode=client"] -> client
      _ -> cdFail $ "Unkown mode for ChatDemo: " ++ show args


-----------------------------------------------------------------------------------------------

-- Utilities

getLeft :: (Data a, Data b, GetChannel c (Either a b)) => c -> IO a
getLeft c = 
  do m <- get c
     case m of Left a -> return a
               Right _ -> getLeft c

getRight :: (Data a, Data b, GetChannel c (Either a b)) => c -> IO b
getRight c = 
  do m <- get c
     case m of Right b -> return b
               Left  _ -> getRight c

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
