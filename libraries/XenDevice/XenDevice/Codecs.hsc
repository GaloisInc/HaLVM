-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- blank line for Haddock/hsc2hs

-- |Encoding and decoding XenStore messages.  This module is used
-- inside HALVMs through the Xenbus, and also from the XenStore
-- daemon.

module XenDevice.Codecs(SockMsgType, ReqId, TxId, ForeignArray,
 Encoder, Decoder, unDecoder, err, lift, uncheckedDecode, encodeMessage, encodeBody0,
 encodeBody, readMessageInternal, decodeMultiBody, decodeSingleBody,
 decodeSingleBody0, ignoreRest, XBMessage) where

import Control.Arrow (first,second)
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Control.Monad
import Control.Exception(assert)
import Control.Monad.Error() -- instance Functor (Either a)

#include <errno.h>
#include "types.h"
#include "xen/io/xs_wire.h"

type SockMsgType = #type uint32_t
type ReqId = #type uint32_t
type TxId = #type uint32_t

-- |A foreign array paired with its size.

type ForeignArray a = (Int,ForeignPtr a)

-- |The type of encoding functions

type Encoder = [String] -> [CChar]

-- |The type of a decoder, which is a monadic parser that either
-- consumes some input and returns a result of type 'a', or just
-- returns an error of type 'e'.

newtype Decoder e a = Decoder ((Int,ForeignArray CChar) -> IO (Either e (Int,a)))

unDecoder :: Decoder a b -> (Int, ForeignArray CChar) -> IO (Either a (Int, b)) 
unDecoder (Decoder f) = f

err :: a -> Decoder a b
err e = Decoder $ const (return (Left e))

instance Functor (Decoder a) where
  fmap f (Decoder g) = Decoder (fmap (fmap (second f)) . g)

binde :: Either e a -> (a -> IO (Either e b)) -> IO (Either e b)
binde v f = either (return . Left) f v 

instance Monad (Decoder e) where
  Decoder f >>= g = Decoder $ \a@(_,p) -> 
                      do fa <- f a
                         binde fa $ \ (o',b) ->
                           do assert (o' <= fst p) $ return ()
                              unDecoder (g b) (o',p)
  return a = Decoder (\ (o,_) -> (return (Right (o,a))))

lift :: IO a -> Decoder e a
lift m = Decoder $ \ (o,_) ->
   do a <- m
      return (Right (o,a))

-- |Decode; don't check for unconsumed input.
uncheckedDecode :: Decoder e a -> ForeignArray CChar -> IO a
uncheckedDecode f p = 
     do r <- unDecoder f (0,p)
        case r of Left _ -> fail "uncheckedDecode"
                  Right (_,a) -> return a

-- |A XenBus message.
type XBMessage = (SockMsgType,ReqId,TxId,ForeignArray CChar)

-- |Encode single message.
encodeMessage :: SockMsgType -> ReqId -> TxId -> [CChar] -> IO (ForeignArray CChar)
encodeMessage msgType reqId txId body = 
  do let body_len = length body
         len = (#size struct xsd_sockmsg)+body_len
     p <- mallocForeignPtrArray len
     withForeignPtr p $ \msg ->
      do (#poke struct xsd_sockmsg,type) msg msgType
         (#poke struct xsd_sockmsg,req_id) msg reqId
         (#poke struct xsd_sockmsg,tx_id) msg txId
         (#poke struct xsd_sockmsg,len) msg body_len
         pokeArray (msg `plusPtr` (#size struct xsd_sockmsg)) body
     return (len,p)
   
-- |Encode a body with null separators and null terminator.
encodeBody0 :: [String] -> [CChar]
encodeBody0 ss = concat $ map (\s -> map castCharToCChar s ++ [nullCChr]) ss
		  
-- |Encode a body with null separators but no null terminator.
encodeBody :: [String] -> [CChar]
encodeBody = init . encodeBody0

-- |Read a single message, given a computation to read a 'ForeignArray'.
readMessageInternal :: (Int -> IO (ForeignArray CChar)) -> IO XBMessage
readMessageInternal read' =
     do (_,fheader) <- read' (#size struct xsd_sockmsg)
        withForeignPtr fheader $ \header -> do
          msgType <- (#peek struct xsd_sockmsg,type) header
          reqId <- (#peek struct xsd_sockmsg,req_id) header
          txId <- (#peek struct xsd_sockmsg,tx_id) header
          body_len <- (#peek struct xsd_sockmsg,len) header
          -- writeDebugConsole ("readXenbus:" ++ (show msgType) ++ " " ++ (show reqId) ++ " " ++ (show txId) ++ " " ++ (show body_len) ++ "\n")
          body <- read' body_len
          return (msgType,reqId,txId,body)

-- |Decode a body containing multiple null-terminated strings.
decodeMultiBody :: Decoder e [String]
decodeMultiBody = Decoder $ \op ->
  withForeignOffsetPtr op (\ (len,b) -> (\a -> Right (len,a)) `fmap` readFields b (b `plusPtr` len))
  where
    readFields p pend
         | p >= pend = return []
         | otherwise =
             do s <- liftM (map castCCharToChar) $ peekArray0n (pend `minusPtr` p) p
	        ss <- readFields (p `plusPtr` (length s + 1)) pend
                return (s:ss)


-- |Decode a body containing a single unterminated string.
decodeSingleBody :: Decoder e String
decodeSingleBody = Decoder $ \op ->
  withForeignOffsetPtr op (\ (len,b) -> (\a -> Right (len,map castCCharToChar a)) `fmap` peekArray len b)

-- |Decode a single null-terminated string.
decodeSingleBody0 :: Decoder e String
decodeSingleBody0 = Decoder $ \op ->
  withForeignOffsetPtr op (\ (len,b) -> (\a -> Right (length a+1,map castCCharToChar a)) `fmap` peekArray0n len b)

-- |Decode a body with no useful content; consumes rest of input.
ignoreRest :: Decoder e ()
ignoreRest = Decoder $ \ (_,(l,_)) -> return (Right (l,()))

nullCChr :: CChar
nullCChr = toEnum 0

-- get a null-terminated string, but don't read more than the size of the array
peekArray0n :: Int -> Ptr CChar -> IO [CChar]
peekArray0n len b = take len `fmap` peekArray0 nullCChr b

withForeignOffsetPtr :: Functor f =>
                        (Int, (Int, ForeignPtr a)) ->
                        ((Int, Ptr b) -> IO (f (Int, c))) ->
                        IO (f (Int, c))
withForeignOffsetPtr (o,(len,body)) f = 
  withForeignPtr body $ \ p -> fmap (first (o+)) `fmap` f (len-o,p `plusPtr` o)
