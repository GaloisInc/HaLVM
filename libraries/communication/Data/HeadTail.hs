{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND

-- | The HeadTail class used for parsing with either strings or bytestrings.
module Data.HeadTail(HeadTail(..)) where

import qualified Data.ByteString.Char8 as BS

class HeadTail l a | l -> a where
  headTail :: l -> Maybe (a,l)

instance HeadTail [a] a where
  headTail [] = Nothing
  headTail (x:xs) = Just (x,xs)

instance HeadTail BS.ByteString Char where
  headTail s | BS.null s    = Nothing
             | otherwise = Just (BS.head s, BS.tail s)
