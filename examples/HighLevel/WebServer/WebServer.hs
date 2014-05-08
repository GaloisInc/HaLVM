{-# LANGUAGE MultiWayIf #-}
-- Copyright 2014 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Hans.Device.Xen
import Hans.DhcpClient
import Hans.NetworkStack hiding (close)
import qualified Hans.NetworkStack as Hans
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore
import Network.HTTP.Base
import Network.HTTP.Stream
import Network.Stream
import System.Exit
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

main :: IO ()
main =
  do con <- initXenConsole
     xs  <- initXenStore

     nics <- listNICs xs
     writeConsole con ("Found " ++ show (length nics) ++ " NIC" ++
                       (if length nics > 1 then "s" else "") ++ "\n")

     mvs <- forM nics $ \ macstr ->
               do writeConsole con ("  " ++ macstr ++ "\n")
                  doneMV <- newEmptyMVar
                  forkFinally (startServer (printer con) xs macstr)
                              (\ _ -> putMVar doneMV ())
                  return doneMV

     -- block until everyone's done
     forM_ mvs takeMVar

startServer :: (String -> IO ()) -> XenStore -> String -> IO ()
startServer print xs macstr =
  do let mac = read macstr
     writeDebugConsole "Starting Server!\n"
     ns <- newNetworkStack
     nic <- openNIC xs macstr
     print ("Starting server on device "++macstr++"\n")
     addDevice ns mac (xenSend nic) (xenReceiveLoop nic)
     writeDebugConsole "deviceUp\n"
     deviceUp ns mac
     writeDebugConsole "new mvar\n"
     ipMV <- newEmptyMVar
     writeDebugConsole "dhcpDiscover\n"
     dhcpDiscover ns mac (putMVar ipMV)
     writeDebugConsole "pulling ip\n"
     ipaddr <- takeMVar ipMV
     print ("Device "++macstr++" has IP "++show ipaddr++"\n")
     lsock <- listen ns undefined 80 `catch` handler
     writeDebugConsole "Got lsock\n"
     forever $ do
       writeDebugConsole "Waiting for connection...\n"
       sock <- accept lsock
       writeDebugConsole "Got a connection! Passing it to a handler!\n"
       forkIO (handleClient print sock)
       return ()
 where
  handler ListenError{} =
    do print ("Unable to listen on port 80\n")
       threadDelay (5 * 1000000)
       exitFailure

handleClient :: (String -> IO ()) -> Socket -> IO ()
handleClient print sock =
  do mreq <- receiveHTTP sock
     writeDebugConsole ("Got request!\n")
     print ("Got request!\n")
     case mreq of
       Left err -> writeDebugConsole ("ReqERROR: " ++ show err ++ "\n")
       Right req -> 
         do let resp = Response {
                         rspCode = (2,0,0)
                       , rspReason = "OK"
                       , rspHeaders = []
                       , rspBody = show req
                       }
            respondHTTP sock resp
            Hans.close sock

printer :: Console -> String -> IO ()
printer target str =
  do writeDebugConsole str
     writeConsole target str

