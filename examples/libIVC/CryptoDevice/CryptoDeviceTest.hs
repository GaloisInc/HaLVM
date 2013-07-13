{-# LANGUAGE ForeignFunctionInterface,ScopedTypeVariables,MultiParamTypeClasses #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Control.Concurrent
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Basics
import Hypervisor.Kernel
import Hypervisor.Memory
import Numeric
import RendezvousLib.ClientServer
import XenDevice.Console
import Communication.RingBuffer
import XenDevice.Xenbus

data CryptoDevRequest =
    EncryptRequest Word32 GrantRef Word32 AES128Key
  | DecryptRequest Word32 GrantRef Word32 AES128Key
  | HashRequest    Word32 GrantRef Word32

data CryptoDevResponse =
    EncryptResponse Word32 Int32
  | DecryptResponse Word32 Int32
  | HashResponse    Word32 Int32 SHA1Hash

connect :: IO (FrontRingBuffer CryptoDevRequest CryptoDevResponse Word32)
(connect, _) = clientServerRingBufferConnections "simple_openssl_device"

main :: IO ()
main = halvm_kernel [dConsole, dXenbus] $ \ _ -> do
  rb <- connect
  writeConsole "Connected to back-end!\n"
  writeConsole "Encryption Password: "
  let dom = frbDomId rb
  passwd <- getLnConsole
  writeConsole $ "Sssh! Here's the password (quoted): '" ++ passwd ++ "'\n"
  (passwdBlockGRef, size) <- stringToBlock dom passwd

  writeConsole "Sending hash request ..."
  key <- hashReq rb $ HashRequest 1 passwdBlockGRef size
  writeConsole $ unlines [ "got it:", '\t' : show key ]

  writeConsole "Enter block to encrypt, followed by '.' on a blank line.\n"
  plainText <- getStringBlock
  (plaintextBlockGRef, ptsize) <- stringToBlock dom plainText

  writeConsole $ unlines [ "Sending block:", '\t':plainText ]

  writeConsole "Sending encrypt request ..."
  encryptReq rb plaintextBlockGRef ptsize $
    EncryptRequest 2 plaintextBlockGRef size key
  writeConsole "done.\n"

  writeConsole "Sending decrypt request ..."
  decryptReq rb plaintextBlockGRef        $
    DecryptRequest 3 plaintextBlockGRef size key
  writeConsole "done.\n"

  threadDelay $ 1000000

  where
    hashReq rb req = do
      writeConsole "Sending frbReq ..."
      resp <- frbRequest rb req
      writeConsole "done.\n"
      case resp of
        HashResponse 1 _ sha -> do
          let key = hashToKey sha
          writeConsole $ "SHA1: " ++ (show sha) ++ "\n"
          writeConsole $ "AES KEY: " ++ (show key) ++ "\n"
          return key
        _ -> do
          writeConsole $ "SHA1 HASH ERROR!\n"
          fail "SHA1 HASH ERROR!\n"

    encryptReq rb gref ptsize req = do
      resp <- frbRequest rb req
      case resp of
        EncryptResponse 2 esize | ptsize <= (fromIntegral esize) -> do
          writeConsole "CIPHERTEXT:\n"
          addr <- grantRefToAddress gref
          arr  <- peekArray (fromIntegral esize) (castPtr addr)
          writeBytes arr 
        EncryptResponse 2 err -> do
          let errstr = "BLOCK ENCRYPTION FAILED (error "++(show err)++")"
          writeConsole $ errstr ++ "\n"
          fail errstr
        _ -> do
          writeConsole "BLOCK ENCRYPTION FAILED!\n"
          fail "BLOCK ENCRYPTION FAILED!\n"

    decryptReq rb gref req = do
      resp <- frbRequest rb req
      case resp of
        DecryptResponse 3 ptsize -> do
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
data SHA1Hash = SHA1Hash [Word8] -- These are not ideal, but work.
data AES128Key = AES128Key [Word8] 

hashToKey :: SHA1Hash -> AES128Key
hashToKey (SHA1Hash x) = AES128Key $ take 16 x

instance Show SHA1Hash where
  show (SHA1Hash x) = concat $ map padconv x

instance Show AES128Key where
  show (AES128Key x) = concat $ map padconv x

padconv :: Word8 -> String
padconv x = if length base == 1 then "0" ++ base else base
 where base = showHex x ""

instance Storable SHA1Hash where
  sizeOf _ = 20
  alignment _ = 1
  peek ptr = SHA1Hash `fmap` peekArray 20 (castPtr ptr)
  poke ptr (SHA1Hash x) = pokeArray (castPtr ptr) x

instance Storable AES128Key where
  sizeOf _ = 16
  alignment _ = 1
  peek ptr = AES128Key `fmap` peekArray 16 (castPtr ptr)
  poke ptr (AES128Key x) = pokeArray (castPtr ptr) x

instance RingBufferable CryptoDevRequest CryptoDevResponse Word32 where
  requestId req = 
    case req of
      EncryptRequest x _ _ _ -> x
      DecryptRequest x _ _ _ -> x
      HashRequest    x _ _   -> x

  responseId resp =
    case resp of
      EncryptResponse x _   -> x
      DecryptResponse x _   -> x
      HashResponse    x _ _ -> x

  entrySize _ _ = 32

encryptKind, decryptKind, hashKind :: Word32
encryptKind = 0xeeeeeeee
decryptKind = 0xdddddddd
hashKind    = 0xabcddcba

instance FrontRingBufferable CryptoDevRequest CryptoDevResponse Word32 where
  peekResponse ptr = do
    kind::Word32 <- peek (castPtr ptr)
    case kind of
      _ | kind == encryptKind -> do -- EncryptResponse
          respid <- peekByteOff (castPtr ptr) 4
          resp <- peekByteOff (castPtr ptr) 8
          return $ EncryptResponse respid resp

        | kind == decryptKind -> do -- DecryptResponse
          respid <- peekByteOff (castPtr ptr) 4
          resp <- peekByteOff (castPtr ptr) 8
          return $ DecryptResponse respid resp

        | kind == hashKind    -> do -- HashResponse
          respid <- peekByteOff (castPtr ptr) 4
          resp <- peekByteOff (castPtr ptr) 8
          hash <- peekByteOff (castPtr ptr) 12
          return $ HashResponse respid resp hash

        | otherwise           ->
          fail "INTERNAL ERROR: REALLY WEIRD RESPONSE!"

  pokeRequest ptr req =
    case req of
      EncryptRequest reqid (GrantRef gref) size key -> do
        pokeByteOff (castPtr ptr) 0  encryptKind
        pokeByteOff (castPtr ptr) 4  reqid
        pokeByteOff (castPtr ptr) 8  ((fromIntegral gref)::Word32)
        pokeByteOff (castPtr ptr) 12 size
        pokeByteOff (castPtr ptr) 16 key

      DecryptRequest reqid (GrantRef gref) size key -> do
        pokeByteOff (castPtr ptr) 0  decryptKind
        pokeByteOff (castPtr ptr) 4  reqid
        pokeByteOff (castPtr ptr) 8  ((fromIntegral gref)::Word32)
        pokeByteOff (castPtr ptr) 12 size
        pokeByteOff (castPtr ptr) 16 key

      HashRequest    reqid (GrantRef gref) size     -> do
        pokeByteOff (castPtr ptr) 0  hashKind
        pokeByteOff (castPtr ptr) 4  reqid
        pokeByteOff (castPtr ptr) 8  ((fromIntegral gref)::Word32)
        pokeByteOff (castPtr ptr) 12 size

instance BackRingBufferable CryptoDevRequest CryptoDevResponse Word32 where
  peekRequest  = undefined
  pokeResponse = undefined


foreign import ccall unsafe "string.h memcpy" 
  memcpy :: Ptr a -> Ptr b -> Word32 -> IO ()
