-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
-- | This module defines an abstraction for configuration spaces.
module Device.PCI.ConfigSpace 
  ( ConfigSpace, configSpace
  , bus, device, function
  , get8, get16, get32
  , set8, set16, set32
  ) where

import Hypervisor.IOPorts

import Numeric
import Data.Word
import Data.Bits



newtype ConfigSpace = C Word16  

bus                :: ConfigSpace -> Word8
bus (C x)           = fromIntegral (x `shiftR` 8)

device             :: ConfigSpace -> Word8 {- Word5 -}
device (C x)        = fromIntegral ((x `shiftR` 3) .&. 31)

function           :: ConfigSpace -> Word8 {- Word3 -}
function (C x)      = fromIntegral x .&. 7

configSpace        :: Word8 -- ^ Bus
                   -> Word8 -- ^ Device (Word5)
                   -> Word8 -- ^ Function (Word3)
                   -> ConfigSpace
configSpace x y z   = C (  fromIntegral   x         `shiftL` 8
                       .|. (fromIntegral (y .&. 31) `shiftL` 3)
                       .|. (fromIntegral (z .&.  7)           )
                        )

instance Show ConfigSpace where
  show x            = showHexPadded 2 (bus x)    $ showString ":"
                    $ showHexPadded 2 (device x) $ showString "."
                    $ showHexPadded 2 (function x) ""

showHexPadded :: (Integral a) => Int -> a -> String -> String
showHexPadded n x s =
  case showHex x "" of
    hex | length hex < n -> take (n - length hex) (repeat '0') ++ hex ++ s
        | otherwise      -> hex ++ s

-- Access to the configuration space -------------------------------------------

get8               :: ConfigSpace -> Word8 -> IO Word8
get8 loc off        = do aim loc off
                         in8 (0xCFC + byteOffset off)

get16              :: ConfigSpace -> Word8 -> IO Word16
get16 loc off       = do aim loc off
                         in16 (0xCFC + wordOffset off)

get32              :: ConfigSpace -> Word8 -> IO Word32
get32 loc off       = do aim loc off
                         in32 0xCFC

set8               :: ConfigSpace -> Word8 -> Word8 -> IO ()
set8 loc off value  = do aim loc off
                         out8 (0xCFC + byteOffset off) value
 
set16              :: ConfigSpace -> Word8 -> Word16 -> IO ()
set16 loc off value = do aim loc off
                         out16 (0xCFC + wordOffset off) value

set32              :: ConfigSpace -> Word8 -> Word32 -> IO ()
set32 loc off value = do aim loc off
                         out32 0xCFC value


-- Private ---------------------------------------------------------------------


aim                :: ConfigSpace -> Word8 -> IO ()
aim (C x) off       = out32 0xCF8 
                    (  1 `shiftL` 31                        -- enable config
                   .|. fromIntegral x `shiftL` 8            -- the space
                   .|. fromIntegral (off .&. complement 3)  -- dword align
                    ) 

byteOffset :: (Bits a, Integral a, Num b) => a -> b
byteOffset x        = fromIntegral (x .&. 3)

wordOffset :: (Bits a, Integral a, Num b) => a -> b
wordOffset x        = fromIntegral (x .&. 2)


