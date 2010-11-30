{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Control.Concurrent
import Control.Monad
import Data.Generics.Basics(Data)
-- import Data.Generics.Aliases(ext1Q, ext1R)
import Data.Typeable(Typeable)
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Hypervisor.Basics
import qualified Communication.IVC as IVC
import Hypervisor.Kernel
import Hypervisor.Memory
import Numeric
import RendezvousLib.PeerToPeer
import XenDevice.Console
import XenDevice.Xenbus

data CryptoDevRequest =
    EncryptRequest GrantRef Word32 AES128Key
  | DecryptRequest GrantRef Word32 AES128Key
  | HashRequest    GrantRef Word32
    deriving ( Typeable, Data )

data CryptoDevResponse =
    EncryptResponse Int32
  | DecryptResponse Int32
  | HashResponse    Int32 SHA1Hash
    deriving ( Typeable, Data )

accept :: IO (IVC.InOutChannel CryptoDevResponse CryptoDevRequest)
(_, accept) = p2pConnection "c_crypt_device"

main :: IO ()
main = halvm_kernel [dConsole, dXenbus] $ \ _ -> do
  chan <- accept
  writeConsole "Connected to back-end!\n"

  writeConsole "Encryption Password: "
  let dom = IVC.peer chan
  (passwdBlockGRef, size) <- getLnConsole >>= stringToBlock dom

  IVC.putBinary chan $ HashRequest passwdBlockGRef size
  key <- handleHash =<< IVC.getBinary chan

  writeConsole "Enter block to encrypt, followed by '.' on a blank line.\n"
  (plaintextBlockGRef, ptsize) <- getStringBlock >>= stringToBlock dom
  writeConsole "Sending block.\n"

  IVC.putBinary chan $ EncryptRequest plaintextBlockGRef size key
  handleEncrypt plaintextBlockGRef ptsize =<< IVC.getBinary chan
  
  IVC.putBinary chan $ DecryptRequest plaintextBlockGRef size key
  handleDecrypt plaintextBlockGRef =<< IVC.getBinary chan

  threadDelay $ 1000000
  
  where
    handleHash resp =
      case resp of
        HashResponse _ sha -> do
          let key = hashToKey sha
          writeConsole $ "SHA1: " ++ (show sha) ++ "\n"
          writeConsole $ "AES KEY: " ++ (show key) ++ "\n"
          return key
        _ -> do
          writeConsole $ "SHA1 HASH ERROR!\n"
          fail "SHA1 HASH ERROR!\n"

    handleEncrypt gref ptsize resp =
      case resp of
        EncryptResponse esize | ptsize <= (fromIntegral esize) -> do
          writeConsole "CIPHERTEXT:\n"
          addr <- grantRefToAddress gref
          writeBytes =<< peekArray (fromIntegral esize) (castPtr addr)
        EncryptResponse err -> do
          let errstr = "BLOCK ENCRYPTION FAILED (error "++(show err)++")"
          writeConsole $ errstr ++ "\n"
          fail errstr
        _ -> do
          writeConsole "BLOCK ENCRYPTION FAILED!\n"
          fail "BLOCK ENCRYPTION FAILED!\n"
 
    handleDecrypt gref resp =
      case resp of
        DecryptResponse ptsize -> do
          writeConsole "PLAINTEXT:\n"
          addr <- grantRefToAddress gref
          str  <- peekCStringLen (castPtr addr, fromIntegral ptsize)
          writeConsole str
        _ -> do
          writeConsole "BLOCK DECRYPTION FAILED!\n"
          fail "BLOCK DECRYPTION FAILED!"
 
writeBytes :: [Word8] -> IO ()
writeBytes [] = return ()
writeBytes x = do
  writeConsole $ "CTEXT: " ++ (concat $ map padconv $ take 32 x) ++ "\n"
  writeBytes $ drop 32 x

stringToBlock :: DomId -> String -> IO (GrantRef, Word32)
stringToBlock dom str = do
  retval <- allocPage 
  ref    <- allocRef
  grantAccess ref dom retval True
  withCStringLen str $ \ (ptr, len) -> do
    let lenW = fromIntegral len
    memcpy retval ptr lenW
    return (ref, lenW)

getStringBlock :: IO String
getStringBlock = do
  cur <- getLnConsole
  if cur == "."
     then return ""
     else do rest <- getStringBlock
             return $ cur ++ "\n" ++ rest  

   
-- Interior type / instance definitions
--
-- These are not ideal, but work
data SHA1Hash = SHA1Hash [Word8] deriving ( Typeable, Data )
data AES128Key = AES128Key [Word8] deriving ( Typeable, Data ) 

hashToKey :: SHA1Hash -> AES128Key
hashToKey (SHA1Hash x) = AES128Key $ take 16 x

instance Show SHA1Hash where
  show (SHA1Hash x) = concat $ map padconv x

instance Show AES128Key where
  show (AES128Key x) = concat $ map padconv x

padconv :: Word8 -> String
padconv x = if length base == 1 then "0" ++ base else base
 where base = showHex x ""

instance Serialize SHA1Hash where
  put (SHA1Hash x) = mapM_ putWord8 x
  get = SHA1Hash `fmap` (sequence $ replicate 20 $ getWord8)

instance Serialize AES128Key where 
  put (AES128Key x) = mapM_ putWord8 x
  get = AES128Key `fmap` (sequence $ replicate 16 $ getWord8)

instance Serialize CryptoDevRequest where
  put x = case x of
            EncryptRequest (GrantRef ref) size key -> do
              putWord16host 0xeeee
              putWord16host ref
              putWord32host size
              put key
            DecryptRequest (GrantRef ref) size key -> do
              putWord16host 0xdddd
              putWord16host ref
              putWord32host size
              put key
            HashRequest (GrantRef ref) size -> do
              putWord16host 0xabcd
              putWord16host ref
              putWord32host size
  get = undefined

instance Serialize CryptoDevResponse where
  put _ = undefined
  get = do kind <- getWord16host
           case kind of
             0xeeee -> (EncryptResponse . fromIntegral) `fmap` getWord32host
             0xdddd -> (DecryptResponse . fromIntegral) `fmap` getWord32host
             0xabcd -> do
               kvar <- remaining -- "kvar" är på Magnus ;-) -- AAM
               unless (kvar == 24) $
                 fail $ "Bad DATA! (rem = " ++ (show kvar) ++ ")"
               resp <- fromIntegral `fmap` getWord32host
               hash <- get
               return $ HashResponse resp hash
             _ ->
               fail "REALLY WEIRD RESPONSE"
              
foreign import ccall unsafe "string.h memcpy" 
  memcpy :: Ptr a -> Ptr b -> Word32 -> IO ()
