-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
--
module DomainBuilder.Utils where

import Data.Bits
import Numeric


-- | How many bits we need to represent the argument.
bytes_to_bits :: Integral a => a -> Int
bytes_to_bits x = ceiling (logBase 2 (fromIntegral x :: Float))


-- | Split a bit-vector into two parts.
split_bits :: Bits a
           => a      -- ^ bit tuple
           -> Int    -- ^ offset (from least significant bit)
           -> (a,a)  -- ^ the separate parts of the tuple
split_bits record offset = (record `shiftR` offset, record .&. mask)
  where mask = (1 `shiftL` offset) - 1


-- | Split a list into chunks of a given size.
-- The last list may be shorter.
chunks :: Int -> [a] -> [[a]]
chunks _ []  = []
chunks n xs  = let (as,bs) = splitAt n xs
               in as : chunks n bs

-- | Computes how many 'b' sized objects we need to fit 'a' units of data.
-- Exmaple: @round_up 7 4 = 2@
-- Because we need 2 4-byte objects to fit 7 bytes.
round_up :: (Integral a) => a -> a -> a
round_up a b = (a + b - 1) `div` b

show_hex :: (Bits a, Integral a) => a -> String
show_hex x = let txt = showHex x ""
                 digs = bitSize x `div` 4
                 pad  = digs - length txt
             in replicate pad '0' ++ txt



