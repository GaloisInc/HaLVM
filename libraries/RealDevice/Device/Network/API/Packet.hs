-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Network.API.Packet (
  module Data.Array.Unboxed,Word8,
  InPacket,len,emptyInPack,toInPack,takeInPack,dropInPack,
  byteAt,wordAt,toChunk,
  OutPacket,outLen,chunks,Chunk,
  emptyOutPack,addChunk,appendOutPack,
  splitOutPack,outBytes,loopback,loopbackout
  ) where

import Device.Bits
import Device.Network.API.Utils
import Data.Array.Unboxed

-- | The buffers used to represent packet,
-- when they are received over the network.
data InPacket = InPack
              { buffer  :: !Chunk
              , from    :: !Int
              , len     :: !Int     -- in bytes
              }
              -- ^Invariant: all (inRange (bounds buffer)) [from..from+len-1]

{-
inPack buf from len
    | len==0 || ok from && ok (from+len-1) = InPack buf from len
    | otherwise = error $ "inPack "++show (bounds buf,from,len)
  where
    ok = inRange (bounds buf)
-}

emptyInPack :: InPacket
emptyInPack = InPack{buffer=empty,from=0,len=0}
  where empty = listArray (0,-1) []

takeInPack :: Int -> InPacket -> InPacket
takeInPack n (InPack buf f l) = InPack buf f (min n l)

dropInPack :: Int -> InPacket -> InPacket
dropInPack n (InPack buf f l) = if n>=l
				     then emptyInPack
				     else InPack buf (f+n) (l-n)

instance Show InPacket where
  show p = "<<"++show (len p)++" bytes>>"

-- | Get a byte at a certain offset.
byteAt             :: InPacket -> Int -> Word8
p `byteAt` x        = buffer p ! (x + from p) 

-- | Get a word from a certain offset (big endian).
wordAt             :: InPacket -> Int -> Word16
p `wordAt` x        = (buf ! off) `nextTo` (buf ! (off + 1))
  where buf         = buffer p
        off         = from p + x

toChunk   :: InPacket -> Chunk
toChunk InPack { buffer=buf,from=i,len=n } =
    if i==lo && n==size
    then buf
  --else listArray (0,n-1) (drop i $ elems $ buffer p)
    else listArray (0,n-1) [buf!j|j<-[first..first+n-1]]
  where
    first=lo+i
    size = hi-lo+1
    (lo,hi) = bounds buf

toInPack :: Chunk -> InPacket
toInPack c = InPack {buffer=c,from=fst (bounds c),len=arraySize c}


-- | The buffers for packets, that are to be sent over the network.
-- Each array contains a header of a layer in the network protocol stack.
data OutPacket = OutPack 
               { outLen  :: !Int
               , chunks  :: ![Chunk]
               }
	       -- ^Invariant: outLen==sum (map arraySize chunks)

instance Show OutPacket where show p = "<<"++show (outLen p)++" bytes>>"

type Chunk = UArray Int Word8
type OutPacketS = OutPacket -> OutPacket

addChunk           :: Chunk -> OutPacketS
addChunk a p        = OutPack { outLen = arraySize a + outLen p , chunks = a : chunks p }

emptyOutPack       :: OutPacket
emptyOutPack        = OutPack { outLen = 0, chunks = [] }

toOutPack :: Chunk -> OutPacket
toOutPack c = OutPack (arraySize c) [c]

appendOutPack      :: OutPacket -> OutPacket -> OutPacket
appendOutPack p1               (OutPack 0  _  ) = p1 -- optimize special case
appendOutPack (OutPack 0  _  ) p2               = p2 -- optimize special case
appendOutPack (OutPack n1 cs1) (OutPack n2 cs2) = OutPack (n1+n2) (cs1++cs2)

{-# NOINLINE splitOutPack #-}
splitOutPack :: Int -> OutPacket -> (OutPacket, OutPacket)
splitOutPack i p@(OutPack n cs) =
    if i>=n
    then (p,emptyOutPack)
    else splitChunks i n cs

{-# NOINLINE splitChunks #-}
splitChunks :: Int -> Int -> [Chunk] -> (OutPacket, OutPacket)
splitChunks 0 n cs = (emptyOutPack,OutPack n cs)
splitChunks _ _ [] = (emptyOutPack,emptyOutPack)
splitChunks i n (c:cs) =
    if i>=n1
    then (toOutPack c,OutPack (n-n1) cs)
    else let (c1,c2) = splitChunk i n1 c
	 in (toOutPack c1,OutPack (n-i) (c2:cs))
  where n1 = arraySize c

{-# NOINLINE splitChunk #-}
splitChunk :: (IArray a3 e, IArray a2 e, Enum a1, Num a1, Ix a1, IArray a e) =>
              a1 -> a1 -> a a1 e -> (a2 a1 e, a3 a1 e)
splitChunk i n c = (listArray (0,i-1) [c!j|j<-[lo..lo+i-1]],
		    listArray (0,n-i-1) [c!j|j<-[lo+i..hi]])
  where (lo,hi) = bounds c

loopback :: OutPacket -> InPacket
loopback p@OutPack{outLen=size} = InPack {buffer=a,from=fst (bounds a),len=size}
  where a = case chunks p of
	      [c] -> c
	      _ -> listArray (0,size-1) (outBytes p)

outBytes :: OutPacket -> [Word8]
outBytes = concatMap elems . chunks

loopbackout :: InPacket -> OutPacket
loopbackout inp = OutPack {outLen=len inp,chunks=[toChunk inp]}
