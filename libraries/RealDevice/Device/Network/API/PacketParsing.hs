-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Network.API.PacketParsing(
  PacketParser,InPacket,doParse,parseInPacket,( #!),( # ),(<# ),
  Parse(..),
  bytes,bits,word8,word16,word32,check8,check16,check,lift,therest,trunc,
  Unparse(..),OutPacket,doUnparse
  ) where

import Control.Monad(liftM,MonadPlus(..),ap)
import Device.Network.API.Packet
import Device.Bits

infixl 1 #,#!,<#

-- | Apply a pure function to the result of a monadic computation
#ifndef __HADDOCK__
(#) :: Functor f => (a -> b) -> f a -> f b
#endif
f # x = fmap f x

-- | Apply a function returned by a monadic computation to an argument returned
-- by a monadic computation
#ifndef __HADDOCK__
(<#) :: Monad m => m (a -> b) -> m a -> m b
#endif
f <# x = ap f x

-- | Perform two monadic computation and return the result from the second one
#ifndef __HADDOCK__
(#!) :: (Monad m, Functor m) => m a -> m b -> m a
#endif
x #! y = const # x <# y


class Parse a where parse :: PacketParser a


newtype PacketParser a = P {unP::In->Maybe (Out a)}
data In = ByteAligned !InPacket
        | Unaligned !Int !InPacket
emptyIn :: In
emptyIn = ByteAligned emptyInPack

data Out a = Out !a !In

doParse :: Parse t => InPacket -> Maybe t
doParse p = parseInPacket parse p

parseInPacket :: PacketParser t -> InPacket -> Maybe t
parseInPacket (P parser) p =
  case parser (ByteAligned p) of
     Just (Out result (ByteAligned p')) | len p'==0 -> Just result
     _ -> Nothing

instance Functor PacketParser where fmap = liftM

instance Monad PacketParser where
  return x = P $ \ p -> return (Out x p)
  P pa >>= xpb = P $ \ p0 -> do Out x p1 <-pa p0
				unP (xpb x) p1
  fail s = P $ \ _ -> fail s

instance MonadPlus PacketParser where
  mzero = fail "no matching alternative"
  P p1 `mplus` P p2 = P $ \ p -> p1 p `mplus` p2 p

instance Parse InPacket where parse = therest
instance Parse (UArray Int Word8) where parse = toChunk # therest

therest :: PacketParser InPacket
therest =
  P $ \ p ->
  case p of
    ByteAligned p' -> return (Out p' emptyIn)
    _ -> fail "The rest is not byte aligned"

trunc :: Int -> PacketParser ()
trunc len' =
  P $ \ p ->
  case p of
    ByteAligned p' ->
      return $ Out () (ByteAligned (takeInPack len' p'))
    _ -> fail "PacketParsing.trunc: the rest is not byte aligned"

instance (Parse a,Parse b) => Parse (a,b) where
  parse = (,) # parse <# parse

instance (Parse a,Parse b,Parse c) => Parse (a,b,c) where
  parse = (,,) # parse <# parse <# parse

instance (Parse a,Parse b,Parse c,Parse d) => Parse (a,b,c,d) where
  parse = (,,,) # parse <# parse <# parse <# parse

instance Parse a => Parse [a] where
  parse = ((:) # parse <# parse) `mplus` return []

instance Parse Bool   where parse = onebit
instance Parse Word8  where parse = word8
instance Parse Word16 where parse = word16
instance Parse Word32 where parse = word32
instance Parse Char   where parse = toEnum.fromIntegral # word8 -- No Unicode!!

-- Bit numbering within bytes is big endian: 0=highest bit, 7=lowest bit
onebit :: PacketParser Bool
onebit =
  P $ \ _p ->
  case _p of
    Unaligned 7 p ->
	if len p==0
	then fail "End of input"
        else return (Out (testBit (byteAt p 0) 0) (ByteAligned (dropInPack 1 p)))
    Unaligned o p -> unaligned o p
    ByteAligned p -> unaligned 0 p
  where
    unaligned o p =
	if len p==0
	then fail "End of input"
        else return (Out (testBit (byteAt p 0) (7-o)) (Unaligned (o+1) p))


-- Could be more efficient for aligned bytes...
bytes :: Num t => t -> PacketParser [Word8]
bytes 0 = return []
bytes n = (:) # word8 <# bytes (n-1)

bits :: (Integral b, Bits b) => Int -> PacketParser b
bits 0 = return 0
bits m =
    if bitcnt==0
    then join # word8 <# bytes (bytecnt-1)
    else join # bits' bitcnt <# bytes bytecnt
  where
    (bytecnt,bitcnt) = m `divMod`  8
    bits' n = -- pre: 1<=n && n<=7
      P $ \ _p ->
      case _p of
	ByteAligned p ->
	    if len p==0
	    then fail "End of input"
	    else return (Out (byteAt p 0 `shiftR` (8-n)) (Unaligned n p))
	Unaligned offset p ->
	  case compare offset' 8 of
	    LT -> if len p==0
		  then fail "End of input"
		  else return (Out (byteAt p 0 `shiftR` (8-offset') .&. mask n)
				   (Unaligned offset' p))
	    EQ -> if len p==0
		  then fail "End of input"
		  else return (Out (byteAt p 0 .&. mask n)
				   (ByteAligned (dropInPack 1 p)))
	    GT -> if len p<2
		  then fail "End of input"
		  else return (Out ((byteAt p 0 .&. mask (8-offset)) `shiftL` o
				    .|. byteAt p 1 `shiftR` (8-o))
				   (Unaligned o (dropInPack 1 p)))
	     where o = offset'-8
	  where offset' = offset+n

    mask n = bit n - 1

    join b1 bs = join' (fromIntegral b1) bs
    join' b1 [] = b1
    join' b1 (b2:bs) = join' (b1 `nextTo` b2) bs

word8 :: PacketParser Word8
word8 =
  P $ \ _p ->
  case _p of
    ByteAligned p ->
        if len p==0
	then fail "End of input"
	else return (Out (byteAt p 0) (ByteAligned (dropInPack 1 p)))
    Unaligned offset p ->
        if len p<2
	then fail "End of input"
	else return (Out out (Unaligned offset (dropInPack 1 p)))
      where out = byteAt p 0 `shiftL` offset .|. byteAt p 1 `shiftR` (8-offset)

word16 :: PacketParser Word16
word16 = nextTo # word8 <# word8 -- network byte order = big endian

word32 :: PacketParser Word32
word32 = nextTo # word16 <# word16 -- network byte order = big endian

check8 :: Word8 -> PacketParser ()
check8 b = check . (==b) =<< word8
check16 :: Word16 -> PacketParser ()
check16 w = check . (==w) =<< word16
check :: Monad m => Bool -> m ()
check b = if b then return () else fail "no parse"
lift :: Monad m => Maybe a -> m a
lift Nothing = fail "no parse"
lift (Just x) = return x

--------------------------------------------------------------------------------

class Unparse a where unparse :: a -> UnparseS

type UnparseS = Unp -> Unp
data Unp = Unp !Int ![Word8] !OutPacket

doUnparse :: Unparse a => a -> OutPacket
doUnparse x = flush (unparse x empty)
  where empty = Unp 0 [] emptyOutPack

instance Unparse Word8 where
  unparse b (Unp cnt bs ps) = Unp (cnt+1) (b:bs) ps

instance Unparse Word16 where
  unparse w (Unp cnt bs ps) = Unp (cnt+2) (b1:b2:bs) ps
    where b1 = w .!. 1
          b2 = w .!. 0

instance Unparse Word32 where
  unparse w (Unp cnt bs ps) = Unp (cnt+4) (b1:b2:b3:b4:bs) ps
    where b1 = w .!. 3
          b2 = w .!. 2
          b3 = w .!. 1
          b4 = w .!. 0

instance Unparse OutPacket where
  unparse p unp = Unp 0 [] (appendOutPack (flush unp) p) -- !!

flush :: Unp -> OutPacket
flush (Unp cnt bs ps) = addChunk (listArray (0,cnt-1) bs) ps

instance Unparse Chunk where
  unparse chunk unp = Unp 0 [] (addChunk chunk (flush unp))

instance Unparse InPacket where
  unparse = unparse . toChunk

instance Unparse () where
  unparse _ = id

instance (Unparse a,Unparse b) => Unparse (a,b) where
  unparse (a,b) = unparse a . unparse b

instance (Unparse a,Unparse b,Unparse c) => Unparse (a,b,c) where
  unparse (a,b,c) = unparse a . unparse b . unparse c

instance (Unparse a,Unparse b,Unparse c,Unparse d) => Unparse (a,b,c,d) where
  unparse (a,b,c,d) = unparse a . unparse b . unparse c . unparse d

instance (Unparse a,Unparse b,Unparse c,Unparse d,Unparse e)
       => Unparse (a,b,c,d,e) where
  unparse (a,b,c,d,e) =
    unparse a . unparse b . unparse c . unparse d . unparse e

instance Unparse a => Unparse [a] where
  unparse [] = id
  unparse (x:xs) = unparse x . unparse xs

instance Unparse Char where
  unparse c = unparse (fromIntegral (fromEnum c)::Word8) -- no Unicode :-(
