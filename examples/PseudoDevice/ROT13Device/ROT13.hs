-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
module ROT13 where

import Data.Char
import Data.Word
import Foreign.Ptr
import Foreign.Storable

rot13encode :: Char -> Char
rot13encode ch | isAsciiLower ch = toLower rot13Upper
               | isAsciiUpper ch = rot13Upper
               | otherwise       = ch
  where
    base, rot13Upper :: Char
    base       = chr $ ord (toUpper ch) + 13
    rot13Upper = if ord base > ord 'Z'
                 then chr (ord (base) - 26)
                 else base

rot13byte :: Word8 -> Word8
rot13byte = fromIntegral . ord . rot13encode . chr . fromIntegral

rot13buffer :: Ptr Word8 -> Word16 -> IO ()
rot13buffer _ 0 = return ()
rot13buffer buffer size =
    do rot13buffer (buffer `plusPtr` 1) (size - 1)
       tmp <- peek buffer
       poke buffer $ rot13byte tmp

