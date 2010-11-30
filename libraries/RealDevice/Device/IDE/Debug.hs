-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
module Device.IDE.Debug where

import Device.IDE.API

import Data.Bits
import Data.Word
import Data.List(intersperse)

import Foreign.Ptr(Ptr)
import Foreign.Storable(peekElemOff)
import Foreign.Marshal.Array(advancePtr)

import qualified Numeric

dump_bits :: Bits a => a -> ([Char], [(Int, [Char])]) -> [Char]
dump_bits w (name,descr) =
  unlines (("-- " ++ name ++ " --") : map dump_bit descr) ++ "-- END --"
  where dump_bit (n,msg) = msg ++ ": " ++ show (w `testBit` n)

status_bits :: Num t => (String, [(t, String)])
status_bits = ("Status"
              , [ (0, "device error")
                , (3, "data request")
                , (6, "device ready")
                , (7, "device busy")
                ]
              )

dma_status_bits :: Num t => (String, [(t, String)])
dma_status_bits = ("DMA status"
                  , [ (0, "DMA active")
                    , (1, "DMA error")
                    , (2, "interrupt status")
                    , (5, "drive 0 is DMA capable")
                    , (6, "drive 1 is DMA capable")
                    ]
                  )

monitor :: (String -> IO a) -> Resources -> IO a
monitor putStrLn r =
  do w <- get_status r
     putStrLn (dump_bits w status_bits)
     w1 <- get_dma_status r
     putStrLn (dump_bits w1 dma_status_bits)
     monitor putStrLn r

dump :: (String -> IO ()) -> Word32 -> Word32 -> Ptr Word16 -> IO ()
dump _ _ n _ | n <= 0 = return ()
dump putStrLn pos n p = do ws <- mapM (peekElemOff p) [0..7]
                           putStrLn (line ws)
                           dump putStrLn (pos + 16) (n - 16) (advancePtr p 8)


  where chars x = [ char (fromIntegral (x .&. 0xFF))
                  , char (fromIntegral (x `shiftR` 8))
                  ]
        char x | x >= 32 && x < 128 = toEnum x
               | otherwise          = '.'

        swap :: Word16 -> Word16
        swap w = (w `shiftL` 8) .|. (w `shiftR` 8)

        line ws = showHex' 7 pos ++ ": " ++
                  concat (intersperse " " (map (showHex' 4 . swap) ws)
                                                ++ ("  " : map chars ws))

        showHex x = Numeric.showHex x []
        showHex' i = reverse . take i . (++repeat '0') . reverse . showHex

