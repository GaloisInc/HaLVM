-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Adam Wick <awick@galois.com>
-- BANNEREND
module Device.Screen.ColorAttribute(
         on
       , ColorAttribute
       , Color(..)
       ) 
 where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable

-- |Create a color attribute given the foreground and background color. This
-- is intended to be used infixed, for clarity: White `on` Black, for example.
on :: Color -> Color -> ColorAttribute
on = ON

-- |A foreground/background combination for printing to the screen.
data ColorAttribute = ON { _fg :: Color, _bg :: Color }

-- |The VGA color attributes.
data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Brown
           | LightGrey
           | DarkGrey
           | LightBlue
           | LightGreen
           | LightCyan
           | LightRed
           | LightMagenta
           | LightBrown
           | White
 deriving (Enum)

instance Storable ColorAttribute where
  alignment _ = 1
  sizeOf _    = 1
  peek p      = do val <- peekWord8 (castPtr p)
                   let fore = val .&. 0xF
                       back = val `shiftR` 4
                       fgc  = toEnum $ fromIntegral fore
                       bgc  = toEnum $ fromIntegral back
                   return $ ON fgc bgc
   where 
    peekWord8 :: Ptr Word8 -> IO Word8
    peekWord8 = peek
  poke p (ON fgc bgc) = do
    let fore = fromIntegral $ fromEnum fgc
        back = fromIntegral $ fromEnum bgc
    poke (castPtr p) (combine fore back)
   where
    combine :: Word8 -> Word8 -> Word8
    combine fore back = (back `shiftL` 4) + fore

