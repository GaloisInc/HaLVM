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
start _args = do
  startNICHandler
  where
    startNICHandler :: IO ()
    startNICHandler = do
      pnics <- potentialNICs
      writeConsole $ "Potential NICs: " ++ (show pnics) ++ "\n"
      if null pnics
         then startNICHandler
         else do
           let (first:_) = pnics
           mnic <- initializeNIC first Nothing
           case mnic of
             Just nic -> do
               writeConsole $ "NIC's name is: " ++ (getNICName nic) ++ "\n"
               let mac = read $ getNICName nic
               setReceiveHandler nic $ handler nic mac
               writeConsole $ "My MAC address is " ++ (show mac) ++ "\n"
             Nothing  -> startNICHandler


handler :: NIC -> MAC -> L.ByteString -> IO ()
handler _ _ bstr | L.length bstr < byteSizePingPacket = return ()
handler nic mac bstr = case decode $ S.concat $ L.toChunks bstr of
  Right packet -> do
    when (etherType packet == 0x2357) $ do
    report $ "Got ping " ++ (show $ pingData packet) ++
             " from " ++ (show $ etherSource packet)
    res <- transmitPacket nic $
             L.fromChunks [ encode $ packet { etherSource = mac
                                            , etherDest   = etherSource packet
                                            , etherType   = 0x7532
                                            }
                          ]
    report $ "Sent packet: " ++ show res
    case res of
      TXOk           -> return ()
      TXPacketTooBig -> report "ERROR: Packet too big!"
      TXBufferFull   -> report "ERROR: Transmit buffer full!"
      TXDropped      -> report "ERROR: Packet was dropped!"
      TXError        -> report "ERROR: Some unknown Xen error!"
      TXOutOfMemory  -> report "ERROR: Out of memory!"
  Left err -> fail err

report :: String -> IO ()
report str = do
  let msg = str ++ "\n"
  writeConsole msg
  writeDebugConsole $ "Server: " ++ msg
