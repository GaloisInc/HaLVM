-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |A simple module -- type class, really -- for converting between host
-- and network order. 
module Data.ByteOrder(ByteOrderConvertable(..)) where

import Data.Int
import Data.Word

class ByteOrderConvertable bc where
    -- |Convert a value (usually a number) from network order to host order.
    fromNetworkOrder :: bc -> bc
    -- |Convert a value (usually a number) from host order to network order.
    toNetworkOrder :: bc -> bc

instance ByteOrderConvertable Word8 where
    fromNetworkOrder x = x
    toNetworkOrder x = x

instance ByteOrderConvertable Word16 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = ((x .&. 0xFF) `shiftL` 8) + ((x .&. 0xFF00) `shiftR` 8)
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif

instance ByteOrderConvertable Word32 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = 
        let (top::Word16) = fromNetworkOrder $ fromIntegral $ x `shiftR` 16
            (bottom::Word16) = fromNetworkOrder $ fromIntegral $ x .&. 0xFFFF
        in (fromIntegral bottom `shiftL` 16) + (fromIntegral top)
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif

instance ByteOrderConvertable Word64 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = 
        let (top::Word32) = fromNetworkOrder $ fromIntegral $ x `shiftR` 32
            (bottom::Word32) = fromNetworkOrder $ fromIntegral $ x .&. 0xFFFFFFFF
        in (fromIntegral bottom `shiftL` 32) + (fromIntegral top)
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif


instance ByteOrderConvertable Int8 where
    fromNetworkOrder x = x
    toNetworkOrder x = x

instance ByteOrderConvertable Int16 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = 
        let (x'::Word16) = fromIntegral x
        in fromIntegral x'
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif

instance ByteOrderConvertable Int32 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = 
        let (x'::Word32) = fromIntegral x
        in fromIntegral x'
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif

instance ByteOrderConvertable Int64 where
#ifdef LITTLE_ENDIAN
    fromNetworkOrder x = 
        let (x'::Word64) = fromIntegral x
        in fromIntegral x'
    toNetworkOrder = fromNetworkOrder
#else
    fromNetworkOrder x = x
    toNetworkOrder x = x
#endif
