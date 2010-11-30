-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.Bits
  ( module Device.Bits
  , module Data.Bits
  , module Data.Word
  ) where

import Data.Bits
import Data.Word

newtype Word1 = Word1 Word8 deriving (Eq,Ord)

instance Show Word1 where show (Word1 x) = show x
instance Read Word1 where
  readsPrec _ ('0':cs)  = [(0,cs)]
  readsPrec _ ('1':cs)  = [(1,cs)]
  readsPrec _ _         = []

instance Num Word1 where
  Word1 x + Word1 y   = Word1 (x `xor` y)
  Word1 x * Word1 y   = Word1 (x .&. y)
  negate x            = x
  abs x               = x
  signum x            = x
  fromInteger x       = Word1 (fromIntegral x .&. 1)

instance Enum Word1 where
  fromEnum (Word1 x)  = fromIntegral x
  toEnum x            = Word1 (fromIntegral x .&. 1)

instance Real Word1 where
  toRational (Word1 x) = fromIntegral x

instance Integral Word1 where
  toInteger (Word1 x) = fromIntegral x
  quotRem _ (Word1 0) = error "Word1: division by 0"
  quotRem x _         = (x,Word1 0)



-- Manipulating bit arrays
-- Warning: this actually uses lazyness :-)

-- 0: least significant (right most in math notation)
(.!.) :: (Bits a, Bits b, Integral a) => a -> Int -> b
array .!. index = let x = fromIntegral (array `shiftR` (bitSize x * index)) in x


nextTo :: (Integral a, Bits a, Bits b, Integral c) => c -> a -> b
b1 `nextTo` b2  = (fromIntegral b1 `shiftL` bitSize b2) .|. fromIntegral b2

byte :: (Bits a, Integral a) => a -> Int -> Word8
byte x n = fromIntegral (x `shiftR` (n * 8))



-- catBits xs              = foldr nextTo 0 xs
catBits                :: (Bits a, Integral a, Bits t) => [a] -> t
catBits bs              = cat bs 0
  where cat [] a        = a
        cat (b:rbs) a   = cat rbs ((a `shiftL` size) .|. fromIntegral b) 
        size            = bitSize (head bs)

