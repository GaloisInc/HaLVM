{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures  #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import BitFiddler.CompactStruct
import Control.Concurrent
import Control.Monad
import Data.Serialize
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Word
import Hypervisor.Kernel
import Hypervisor.Debug
import XenDevice.Console
import XenDevice.NIC
import Common


$(generateCompactStruct "PingPacket" True [
   ("etherDest",        48, [t| MAC |]),
   ("etherSource",      48, [t| MAC |]),
   ("etherType",        16, [t| Word16 |]),
   ("pingData",         32, [t| Word32 |])
 ])

main :: IO ()
main = halvm_kernel_daemon [dConsole, dNICs] start

start :: [String] -> IO ()
start args =
  startNICHandler
  where
    startNICHandler :: IO ()
    startNICHandler = do
      pnics <- potentialNICs
      if null pnics
         then startNICHandler
         else do
           let (first:_) = pnics
           mnic <- initializeNIC first Nothing
           case mnic of
             Just nic -> do
               let mac = read $ getNICName nic
               setReceiveHandler nic $ handler nic mac
               writeConsole $ "My MAC address is " ++ show mac ++ "\n"
               forM_ [1..rounds] $ pingSlowly mac nic
             Nothing  -> startNICHandler

    rounds :: Word32
    rounds = case args of
               [arg] | take 7 arg == "rounds=" -> read $ drop 7 arg
               _                               -> 100

pingSlowly :: MAC -> NIC -> Word32 -> IO ()
pingSlowly mac nic num = do
  let packet = PingPacket {
                 etherSource = mac
               , etherDest   = read "ff:ff:ff:ff:ff:ff" -- Ethernet broadcast
               , etherType   = 0x2357
               , pingData    = num
               }
  writeDebugConsole "Pinger: pinging ...\n"
  res <- transmitPacket nic $ L.fromChunks [encode packet]
  writeDebugConsole "Pinger: pinged\n"
  case res of
    TXOk           -> report $ "Sent ping #" ++ show num ++ "\n"
    TXPacketTooBig -> writeError "Packet too big"
    TXBufferFull   -> writeError "Buffer full"
    TXDropped      -> writeError "Packet dropped"
    TXError        -> writeError "Unknown Xen error"
    TXOutOfMemory  -> writeError "Out of memory"
  threadDelay 1000000 -- one second
  -- pingSlowly mac nic (num + 1)
  where
    writeError :: String -> IO ()
    writeError str =
      report $ "Failed to write packet " ++ show num ++ ": " ++ str

handler :: NIC -> MAC -> L.ByteString -> IO ()
handler _ _ bstr | L.length bstr < byteSizePingPacket = return ()
handler _ mac bstr = case decode $ S.concat $ L.toChunks bstr of
  Right packet -> do
    when (etherType packet == 0x7532 && etherDest packet == mac) $ do
    report $ "Received response " ++ (show $ pingData packet) ++
             " from " ++ (show $ etherSource packet)
  Left err -> fail err


report :: String -> IO ()
report str = do
  let msg = str ++ "\n"
  writeConsole msg
  writeDebugConsole $ "Pinger: " ++ msg
