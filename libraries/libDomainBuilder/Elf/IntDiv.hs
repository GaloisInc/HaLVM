-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module Elf.IntDiv where
import qualified Elf.Product_Type

adjust :: Integer -> (Integer, Integer) -> (Integer, Integer)
adjust b a = let
               (qa, ra) = a
             in (if 0 <= ra - b then (2 * qa + 1, ra - b) else (2 * qa, ra))

negDivAlg :: (Integer, Integer) -> (Integer, Integer)
negDivAlg (a, b) = (if 0 <= a + b || b <= 0 then ((negate 1), a + b) else Elf.IntDiv.adjust b (Elf.IntDiv.negDivAlg (a, 2 * b)))

negateSnd :: (Integer, Integer) -> (Integer, Integer)
negateSnd a = let
                (qa, ra) = a
              in (qa, negate ra)

posDivAlg :: (Integer, Integer) -> (Integer, Integer)
posDivAlg (a, b) = (if a < b || b <= 0 then (0, a) else Elf.IntDiv.adjust b (Elf.IntDiv.posDivAlg (a, 2 * b)))

divAlg :: (Integer, Integer) -> (Integer, Integer)
divAlg a = let
             (aa, ba) = a
           in (if 0 <= aa then (if 0 <= ba then Elf.IntDiv.posDivAlg (aa, ba) else (if aa == 0 then (0, 0) else Elf.IntDiv.negateSnd (Elf.IntDiv.negDivAlg (negate aa, negate ba)))) else (if 0 < ba then Elf.IntDiv.negDivAlg (aa, ba) else Elf.IntDiv.negateSnd (Elf.IntDiv.posDivAlg (negate aa, negate ba))))

op_mod_int :: Integer -> Integer -> Integer
op_mod_int a b = Elf.Product_Type.snd (Elf.IntDiv.divAlg (a, b))
