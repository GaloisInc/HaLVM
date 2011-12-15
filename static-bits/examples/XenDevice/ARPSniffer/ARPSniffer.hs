{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Word
import Numeric
import Hypervisor.Kernel
import Hypervisor.Debug
import XenDevice.Console
import XenDevice.NIC

data MAC = MAC Word8 Word8 Word8 Word8 Word8 Word8
data IP = IP Word8 Word8 Word8 Word8

$(generateCompactStruct "EtherARPPacket" True [
   ("etherDest",        48, [t| MAC |]),
   ("etherSource",      48, [t| MAC |]),
   ("etherType",        16, [t| Word16 |]),
   ("arpHWType",        16, [t| Word16 |]),
   ("arpProtocol",      16, [t| Word16 |]),
   ("arpHWByteSize",     8, [t| Word8 |]),
   ("arpProtByteSize",   8, [t| Word8 |]),
   ("arpType",          16, [t| Word16 |]),
   ("arpSenderEth",     48, [t| MAC |]),
   ("arpSenderIP",      32, [t| IP |]),
   ("arpReceiverEth",   48, [t| MAC |]),
   ("arpReceiverIP",    32, [t| IP |])
 ])

main :: IO ()
main = halvm_kernel_daemon [dConsole, dNICs] start

start :: [String] -> IO ()
start _args = startNICHandler
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
             Just nic -> setReceiveHandler nic handler
             Nothing  -> startNICHandler

handler :: L.ByteString -> IO ()
handler bstr | L.length bstr < byteSizeEtherARPPacket = return ()
handler bstr = case decode $ S.concat $ L.toChunks bstr of
  Right packet -> do
    when (etherType packet == 0x0806) $ do
      let info =
            case arpType packet of
              1 -> (show $ arpSenderIP packet) ++ " REQUESTs " ++ 
                   (show $ arpReceiverIP packet)
              2 -> (show $ arpSenderIP packet) ++ " RESPONDs " ++ 
                   (show $ arpReceiverEth packet) ++ " for " ++ 
                   (show $ arpReceiverIP packet)
              _ -> 
                   "UNKNOWN TYPE (" ++ showHex (arpType packet) "" ++ ")"
      writer $ (show $ etherSource packet) ++ " -> " ++
               (show $ etherDest packet) ++ ": " 
               ++ info ++ "\n"
  Left err -> fail err

writer :: String -> IO ()
writer s = do
  writeConsole s
  writeDebugConsole s

instance Serialize MAC where 
  get = replicateM 6 get >>= (\ [a,b,c,d,e,f] -> return $ MAC a b c d e f)
  put (MAC a b c d e f) = mapM_ put [a,b,c,d,e,f]

instance Serialize IP where
  get = replicateM 4 get >>= (\ [a,b,c,d] -> return $ IP a b c d)
  put (IP a b c d) = mapM_ put [a,b,c,d]

instance Show MAC where
    show (MAC a b c d e f) = 
        (showHex a "") ++ ":" ++ (showHex b "") ++ ":" ++ 
        (showHex c "") ++ ":" ++ (showHex d "") ++ ":" ++ 
        (showHex e "") ++ ":" ++ (showHex f "")

instance Show IP where
    show (IP a b c d) =
        (show a) ++ "." ++ (show b) ++ "." ++ (show c) ++ "." ++ (show d)

