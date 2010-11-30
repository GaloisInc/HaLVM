-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module Elf.Do where

mkBlind :: (a -> (c -> b) -> b) -> a -> b -> b
mkBlind f a b = f a (\ _ -> b)
