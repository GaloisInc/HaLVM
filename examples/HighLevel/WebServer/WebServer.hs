{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- Copyright 2014 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Data.Time
import Data.Version
import Data.Word
import GHC.Stats
import Hans.Address.IP4
import Hans.Device.Xen
import Hans.DhcpClient
import Hans.Layer.Dns(DnsException)
import Hans.NetworkStack hiding (close)
import qualified Hans.NetworkStack as Hans
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Stream
import Network.Socket.Internal(getNetworkHansStack)
import Network.Stream
import System.Exit
import System.Info
import System.Locale
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes(href)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Internal(string)
import XenDevice.NIC

instance Stream Socket where
  readLine s = loop ""
   where loop acc =
           do bstr <- recvBytes s 1
              if | BS.null bstr       -> return (Left ErrorClosed)
                 | BS.head bstr == 10 -> return (Right (acc ++ "\n"))
                 | otherwise          -> loop (acc ++ BSC.unpack bstr)

  readBlock s x = loop (fromIntegral x) BS.empty
    where loop 0 acc = return (Right (BSC.unpack acc))
          loop x acc =
            do bstr <- recvBytes s x
               if | BS.length bstr == x -> loop 0 (acc `BS.append` bstr)
                  | BS.length bstr == 0 -> return (Left ErrorClosed)
                  | otherwise           -> loop (x - BS.length bstr)
                                                (acc `BS.append` bstr)

  writeBlock s str = loop (BSC.pack str)
    where loop x | BS.null x = return (Right ())
                 | otherwise =
                    do amt <- sendBytes s x
                       loop (BS.drop amt x)

  close s = Hans.close s

  closeOnEnd _ _ = return ()

data ServerState = ServerState {
    startTime     :: String
  , responseCount :: MVar Word64
  , lastHosts     :: MVar [(IP4, Maybe String)]
  }

main :: IO ()
main =
  do con <- initXenConsole
     xs  <- initXenStore

     startT <- formatTime defaultTimeLocale "%c" `fmap` getZonedTime
     respC  <- newMVar 0
     lastH  <- newMVar []
     let state = ServerState startT respC lastH

     nics <- listNICs xs
     writeConsole con ("Found " ++ show (length nics) ++ " NIC" ++
                       (if length nics > 1 then "s" else "") ++ "\n")

     mvs <- forM nics $ \ macstr ->
               do writeConsole con ("  " ++ macstr ++ "\n")
                  doneMV <- newEmptyMVar
                  forkFinally (startServer (printer con) xs macstr state)
                              (\ _ -> putMVar doneMV ())
                  return doneMV

     -- block until everyone's done
     forM_ mvs takeMVar

startServer :: (String -> IO ()) -> XenStore -> String -> ServerState -> IO ()
startServer print xs macstr state =
  do let mac = read macstr
     ns <- newNetworkStack
     nic <- openNIC xs macstr
     print ("Starting server on device "++macstr++"\n")
     addDevice ns mac (xenSend nic) (xenReceiveLoop nic)
     deviceUp ns mac
     ipMV <- newEmptyMVar
     dhcpDiscover ns mac (putMVar ipMV)
     ipaddr <- takeMVar ipMV
     print ("Device "++macstr++" has IP "++show ipaddr++"\n")
     lsock <- listen ns undefined 80 `catch` handler
     forever $ do
       sock <- accept lsock
       writeDebugConsole ("Accepted socket.\n");
       forkIO (handleClient print sock state)
       forkIO (addHost ns (sockRemoteHost sock) (lastHosts state))
       return ()
 where
  handler ListenError{} =
    do print ("Unable to listen on port 80\n")
       threadDelay (5 * 1000000)
       exitFailure

handleClient :: (String -> IO ()) -> Socket -> ServerState -> IO ()
handleClient print sock state =
  do mreq <- receiveHTTP sock
     case mreq of
       Left err -> writeDebugConsole ("ReqERROR: " ++ show err ++ "\n")
       Right req ->
         do body <- buildBody print req state
            print ("Built response\n")
            let lenstr = show (length body)
                keepAlive = [ mkHeader HdrConnection "keep-alive"
                            | hdr <- retrieveHeaders HdrConnection req
                            , map toLower (hdrValue hdr) == "keep-alive" ]
                conn | null keepAlive = [ mkHeader HdrConnection "Close" ]
                     | otherwise      = keepAlive
                resp = Response {
                             rspCode = (2,0,0)
                           , rspReason = "OK"
                           , rspHeaders = mkHeader HdrContentLength lenstr
                                        : mkHeader HdrContentType   "text/html"
                                        : conn
                           , rspBody = body
                           }
            respondHTTP sock resp
            if null keepAlive
               then Hans.close sock
               else handleClient print sock state

buildBody :: (String -> IO ()) -> Request String -> ServerState -> IO String
buildBody print req state =
  do numReqs <- modifyMVar (responseCount state) (\ x -> return (x + 1, x))
     prevHosts <- readMVar (lastHosts state)
     writeDebugConsole ("prevHosts: " ++ show prevHosts ++ "\n")
     return $ renderHtml $
       docTypeHtml $ do
         H.head (title "HaLVM Test Server")
         body $ do
           h1 "Hi!"
           p $ do "I'm a HaLVM. Technically I'm " >> string compilerName
                  " version " >> string (showVersion compilerVersion)
                  ", ported to run on Xen. I started running on "
                  string (startTime state) >> ". You didn't know there was a "
                  "HaLVM standard time, did you? Well, there is. Really."
           p $ do "I have responded to " >> (string (show numReqs)) >> " "
                  "requests since I started, not including yours."
           p $ do "I am using: "
           ul $ do
             li $    hans >> " to talk to you over TCP."
             li $ do http >> " (modified to use the " >> network_hans
                     " shim) to understand your request and respond to it."
             li $ do blaze >> " (which built out of the box) to generate this " 
                     "pretty HTML."
             li $ do "... and a host of other libraries to do other, more "
                     "standard, things."
           p $ do "Here are the last 5 hosts that I did something for:"
           ol $ forM_ prevHosts $ \ x -> li (renderHost x)
 where
  hans, http, blaze, network_hans :: Html
  hans  = a ! href "http://hackage.haskell.org/package/hans" $ "HaNS"
  http  = a ! href "http://hackage.haskell.org/package/HTTP" $ "HTTP"
  blaze = a ! href "http://hackage.haskell.org/package/blaze-html"$"blaze-html"
  network_hans = a ! href "http://github.com/GaloisInc/network-hans" $
                   "network-hans"

renderHost :: (IP4, Maybe String) -> Html
renderHost (addr, Nothing) = string (show addr)
renderHost (addr, Just n) = string (n ++ " (" ++ show addr ++ ")")

addHost :: NetworkStack -> IP4 -> MVar [(IP4, Maybe String)] -> IO ()
addHost ns addr hostsMV =
  do writeDebugConsole ("addHost " ++ show addr ++ "\n")
     entry <- catch (name `fmap` getHostByAddr ns addr) handleDnsError
     list <- takeMVar hostsMV
     writeDebugConsole ("list: " ++ show list ++ "\n")
     case lookup addr list of
       Just _  -> putMVar hostsMV list >> writeDebugConsole ("put old list\n")
       Nothing -> putMVar hostsMV (take 5 ((addr, entry) : list )) >> writeDebugConsole ("put new list\n")
 where
  name = Just . hostName
  handleDnsError :: DnsException -> IO (Maybe String)
  handleDnsError e = writeDebugConsole (show e ++ "\n") >> return Nothing

printer :: Console -> String -> IO ()
printer target str =
  do writeDebugConsole str
     writeConsole target str

