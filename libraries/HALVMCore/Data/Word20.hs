{-# OPTIONS -fglasgow-exts #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>
-- BANNEREND

-- |An implementation of 20 bit unsigned words. These are used in x86 
-- frame numbers. There's nothing particularly interesting here beyond
-- its existence.
module Data.Word20(Word20) where

import Data.Word(Word32)
import Data.Bits
import Data.Ix
import Data.Ratio
import Foreign.Storable(Storable)
import Data.Generics(Typeable, Data)

newtype Word20 = Word20 {unWord20 :: Word32}
  deriving (Eq,Ord,Storable, Typeable, Data)


-- for other bit lengths, just change these definitions
type T = Word20
bits :: Int  
bits = 20
wrap :: Word32 -> Word20
wrap = Word20
unwrap :: Word20 -> Word32
unwrap = unWord20
maxVal :: Word32
maxVal = (1 `shiftL` bits) - 1
narrow :: Word32 -> Word32
narrow w = w .&. maxVal

instance Show Word20 where
  showsPrec p x = showsPrec p (unwrap x)

instance Num Word20 where
  x + y = wrap (narrow (unwrap x + unwrap y))
  x - y = wrap (narrow (unwrap x - unwrap y))
  x * y = wrap (narrow (unwrap x * unwrap y))
  negate x = wrap (narrow (negate (unwrap x)))
  abs x = x
  signum x | unwrap x == 0  = 0
  signum _                  = 1
  fromInteger i = wrap (narrow(fromInteger i))

instance Bounded Word20 where
  minBound = 0
  maxBound = wrap maxVal

instance Real Word20 where
  toRational x = toInteger (unwrap x) % 1

{- following Enum support stuff stolen from GHC libraries -}

{-# NOINLINE toEnumError #-}
toEnumError :: (Show a) => String -> Int -> (a,a) -> b
toEnumError inst_ty i bnds =
    error $ "Enum.toEnum{" ++ inst_ty ++ "}: tag (" ++
            show i ++
            ") is outside of bounds " ++
            show bnds

{-# NOINLINE succError #-}
succError :: String -> a
succError inst_ty =
    error $ "Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound"

{-# NOINLINE predError #-}
predError :: String -> a
predError inst_ty =
    error $ "Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound"

integralEnumFrom :: (Integral a, Bounded a) => a -> [a]
integralEnumFrom n = map fromInteger [toInteger n .. toInteger (maxBound `asTypeOf` n)]

integralEnumFromThen :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThen n1 n2
  | i_n2 >= i_n1  = map fromInteger [i_n1, i_n2 .. toInteger (maxBound `asTypeOf` n1)]
  | otherwise     = map fromInteger [i_n1, i_n2 .. toInteger (minBound `asTypeOf` n1)]
  where
    i_n1 = toInteger n1
    i_n2 = toInteger n2

integralEnumFromTo :: Integral a => a -> a -> [a]
integralEnumFromTo n m = map fromInteger [toInteger n .. toInteger m]

integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]
integralEnumFromThenTo n1 n2 m
  = map fromInteger [toInteger n1, toInteger n2 .. toInteger m]

instance Enum Word20 where
  succ x
      | x /= maxBound = x + 1
      | otherwise     = succError ("Word" ++ show bits)
  pred x
      | x /= minBound = x - 1
      | otherwise     = predError ("Word" ++ show bits)
  toEnum i
      | i >= 0 && i <= fromIntegral (maxBound::T)
                      = wrap (toEnum i)
      | otherwise     = toEnumError ("Word" ++ show bits) i (0,maxBound::T)
  fromEnum x = fromEnum (unwrap x) 
  enumFrom            = integralEnumFrom
  enumFromThen        = integralEnumFromThen
  enumFromTo          = integralEnumFromTo
  enumFromThenTo      = integralEnumFromThenTo


{-# RULES
"fromIntegral/Word20->Word32" fromIntegral = fromIntegral . unwrap
"fromIntegral/Word20->Int"    fromIntegral = fromIntegral . unwrap
"fromIntegral/Word20->Word20" fromIntegral = id :: Word20 -> Word20
  #-}

instance Ix Word20 where
  range (m,n)                    = [m..n]
  index b@(m,_) i  | inRange b i = fromIntegral (i - m)
                   | otherwise   = error "Error in array index"
  inRange (m,n) i                = m <= i && i <= n

instance Read Word20 where
  readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s] 

instance Integral Word20 where
  x `quot` y = wrap(unwrap x `quot` unwrap y)
  x `rem` y = wrap(unwrap x `rem` unwrap y)
  x `div` y = wrap(unwrap x `div` unwrap y)
  x `mod` y = wrap(unwrap x `mod` unwrap y)
  quotRem x y = (wrap q,wrap r) where (q,r) = quotRem (unwrap x) (unwrap y)
  divMod x y = (wrap d,wrap m) where (d,m) = divMod (unwrap x) (unwrap y)
  toInteger x = toInteger (unwrap x)

instance Bits Word20 where
  x .&. y = wrap (unwrap x .&. unwrap y)
  x .|. y = wrap (unwrap x .|. unwrap y)
  x `xor` y = wrap (unwrap x `xor`  unwrap y)
  complement x = wrap (unwrap x `xor` maxVal)
  x `shift` i = wrap (narrow (unwrap x `shift` i))
  x `rotate` i 
     | i == 0    = x
     | otherwise = wrap (narrow ((x' `shiftL` i') .|. (x' `shiftR` (bits - i'))))
     where x' = unwrap x
           i' = i `mod` bits -- always positive
  bitSize  _               = bits
  isSigned _               = False
  

