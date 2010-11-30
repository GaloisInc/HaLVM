-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module Elf.Product_Type where

snd :: (b, a) -> a
snd (_, y) = y

fst :: (a, b) -> a
fst (y, _) = y
