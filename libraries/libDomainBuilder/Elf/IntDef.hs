-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module Elf.IntDef where

nat_aux :: Integer -> Integer -> Integer
nat_aux n i = (if i <= 0 then n else Elf.IntDef.nat_aux (n + 1) (i - 1))

nat :: Integer -> Integer
nat k = (if k < 0 then 0 else k)
--nat i = Elf.IntDef.nat_aux 0 i

eq_bit :: Bool -> Bool -> Bool
eq_bit False False = True
eq_bit True True = True
eq_bit False True = False
eq_bit True False = False

int_aux :: Integer -> Integer -> Integer
int_aux i n = (if n == 0 then i else Elf.IntDef.int_aux (i + 1) (n - 1))
-- int_aux b na = (if na == 0 then b else Elf.IntDef.int_aux (b + 1) (Elf.IntDef.nat (na - 1)))
