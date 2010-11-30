-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.IDE.Info where

import Data.Array.Unboxed

import Data.Maybe(mapMaybe)
import Data.Bits
import Data.Word
import Numeric(showFFloat)


sector_size :: Word16
sector_size = 512

data Info = Info
  { is_ata           :: Bool
  , removable_device :: Bool
  , removable_media  :: Bool
  , serial_number    :: String
  , firmware_rev     :: String
  , model_number     :: String
  , total_sectors    :: Word64
  , max_dma_mode     :: Int
  , lba48            :: Bool
  , features         :: Maybe [String]
  } -- we can parse more, if neccessary.
    deriving Show

pp_info :: Info -> [String]
pp_info i = [ p "ata" is_ata
            , p "removable device" removable_device
            , p "removable media" removable_media
            , p "total sectors" total_sectors ++ " (" ++ showFFloat (Just 2) size_mb "M)"
            , p "serial number" serial_number
            , p "firmware revision" firmware_rev
            , p "model number" model_number
            , p "max dma mode" max_dma_mode
            , p "48 bit LBA" lba48
            , p "supported features" features
            ]
  where p x f = x ++ ": " ++ show (f i)
        size_mb :: Double
        size_mb = fromIntegral (total_sectors i * fromIntegral sector_size) / (1024 * 1024)



total_bytes :: Info -> Word64
total_bytes info  = total_sectors info * fromIntegral sector_size

parse_device_info :: UArray Word8 Word16 -> Info
parse_device_info arr = Info
  { is_ata            = not ((arr ! 0) `testBit` 15)
  , removable_device  = not ((arr ! 0) `testBit` 6)
  , removable_media   = (arr ! 0) `testBit` 7
  , serial_number     = ascii_chars 10 19
  , firmware_rev      = ascii_chars 23 26
  , model_number      = ascii_chars 27 46
  , total_sectors     = if supports_lba48 then word64 100 else fromIntegral (word32 60)
  , max_dma_mode      = let w = arr ! 63
                        in case w .&. 7 of
                             1 -> 0
                             3 -> 1
                             _ -> 2
  , lba48 = supports_lba48
  , features = let w1 = arr ! 82
                   w2 = arr ! 83
                   check1 w (n,x) = if w `testBit` n then Just x else Nothing
                   check w xs = mapMaybe (check1 w) (zip [0..] xs)
               in if w1 == 0 || w1 == 0xFFFF || w2 == 0 || w2 == 0xFFFF
                    then Nothing
                    else Just (
                          check w1
                          [ "SMART feature set"
                          , "Security Mode feature set"
                          , "Removable Media feature set"
                          , "Mandatory Power Management feature set"
                          , "PACKET command"
                          , "write cache"
                          , "look-ahead"
                          , "release interrupt"
                          , "SERVICE interrupt"
                          , "DEVICE RESET command"
                          , "Host Protected Area"
                          , "?"
                          , "WRITE BUFFER command"
                          , "READ BUFFER command"
                          , "NOP command"
                          , "?"
                          ] ++
                          check w2
                          [ "DOWNLOAD MICROCODE command"
                          , "READ/WRITE DMA QUEUED"
                          , "CFA feature set"
                          , "Advanced Power Managment features are"
                          , "Removable Media Status Notification feature set"
                          , "Power-Up in Standby feature set"
                          , "SET FEATURES subcommand required to spin up after power up"
                          , "???"
                          , "SET MAX security extension"
                          , "Automatic Acoustic Management feature set"
                          , "48-bit Addresses feature set"
                          , "Device Configuration Overlay feature set"
                          , "mandatory FLUSH CACHE command"
                          , "FLUSH CACHE EXT command"
                          ])


  }

  where
  supports_lba48 = (arr ! 83) `testBit` 10

  word32 :: Word8 -> Word32
  word32 x = (fromIntegral (arr ! (x+1)) `shiftL` 16) .|. fromIntegral (arr ! x)

  word64 :: Word8 -> Word64
  word64 x = fromIntegral (word32 (x + 2) `shiftL` 32) .|. fromIntegral (word32 x)

  ascii_chars :: Word8 -> Word8 -> String
  ascii_chars from to = concatMap chars (map (arr !) [from .. to ])

  chars :: Word16 -> String
  chars w = [ toEnum (fromIntegral (w `shiftR` 8))
            , toEnum (fromIntegral (w .&. 0xFF)) ]


