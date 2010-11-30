-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module PCI.ParseVendors 
  ( parseTables 
  , VendorTable, DeviceTable, SubSysTable
  , ClassTable, SubClassTable, ProgIFTable
  ) where

  
-- import ParsOps2

import Numeric(readHex)
import Prelude hiding (lines)
import Data.Array.Unboxed
import Data.Map
import Data.Word
import Data.PackedString

type VendorTable    = Word16 -> Maybe (PackedString, DeviceTable)
type DeviceTable    = Word16 -> Maybe (PackedString, SubSysTable)
type SubSysTable    = (Word16,Word16) -> Maybe PackedString
type ClassTable     = Word8 -> Maybe (PackedString, SubClassTable)
type SubClassTable  = Word8 -> Maybe (PackedString, ProgIFTable)
type ProgIFTable    = Word8 -> Maybe PackedString

parseTables        :: UArray Int Word8 -> Maybe (VendorTable, ClassTable)
parseTables arr     = let (lo,hi) = bounds arr
                      in case tstparse records (B lo hi arr) of
                           Left _ -> Nothing
                           Right ((v,c),_) -> Just (v,c)

pVendor   = pair3 # tok (pHexNum 4) <# pTxt <# pFM pDevice
pDevice   = pair3 # (tab >> tok (pHexNum 4)) <# pTxt <# pFM pSub
pSub      = mk  # (tab >> tab >> tok (pHexNum 4)) <# tok (pHexNum 4) <# pTxt
  where mk x y z = ((x,y),z)

pClass    = pair3 # (tok (char 'C') >> tok (pHexNum 2)) <# pTxt <# pFM pSubClass
pSubClass = pair3 # (tab >> tok (pHexNum 2)) <# pTxt <# pFM pProgIF
pProgIF   = (,)   # (tab >> tab >> tok (pHexNum 2)) <# pTxt

records   = (,)   # pFM pVendor <# pFM pClass

pFM p     = do xs <- lines p
               return (lookupFM (listToFM xs))

pair3 x y z         = (x,(y,z))
f <# x              = f `ap` x
f # x               = fmap f x                        
tab                 = char '\t'
newline             = char '\n'
char x              = lit (ord x)
tok p               = do x <- p; spaces; return x
comment             = char '#' >> pTxt >> newline
blank               = many (char ' ') >> newline
spaces              = some (char ' ')
ignore              = many (comment `orelse` blank)
lines p             = many (line p)
line p              = ignore >> p 
pTxt                = do xs <- many (scan' "character" (not . (ord '\n' ==)))
                         return (packString (map chr xs))
isBetween x y z     = x <= z && z <= y
isHexDigit x        = isBetween (ord '0') (ord '9') x
                   || isBetween (ord 'a') (ord 'f') x

ord                :: Char -> Word8
ord c               = fromIntegral (fromEnum c)

chr                :: Word8 -> Char
chr x               = toEnum (fromIntegral x)

pHexNum            :: Integral a => Int -> Parser Word8 a
pHexNum n           = do n <- repeatP n (scan' "hex digit" (isHexDigit))
                         case readHex (map chr n) of
                           [(n,"")] -> return n
                           _        -> failP "hex number"

                          

