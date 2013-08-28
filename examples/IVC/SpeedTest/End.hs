{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Communication.IVC
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Time
import Data.Word
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore

import Common

main :: IO ()
main = do
  xs <- initXenStore
  con <- initXenConsole
  args <- alistArgs
  let Just inName = lookup "inchan" args
  writeDebugConsole "END: Building input channel.\n"
  let (_, makeInChan) = connectionBuilders inName
  ichan <- makeInChan xs
  writeDebugConsole "END: Built input channel.\n"
  get ichan
  writeDebugConsole "END: Got first item.\n"
  start <- getCurrentTime
  writeDebugConsole "END: Got current time.\n"
  run_getter con 0 0 start ichan

run_getter :: Console ->
              Word64 -> Word64 ->
              UTCTime -> InChannel ByteString ->
              IO ()
run_getter con !total !x !start_t !c
  | x >= print_amt = do print_speed con start_t total
                        run_getter con total 0 start_t c
  | otherwise      = do block <- get c
                        let !size = fromIntegral $ BS.length block
                        run_getter con (total + size) (x + size) start_t c
 where
  print_amt = 128 * 1024 * 1024

print_speed :: Console -> UTCTime -> Word64 -> IO ()
print_speed con start numBytes = do
  now <- getCurrentTime
  let !diff     = diffUTCTime now start
      !diff_rat = toRational diff :: Rational
      !diff_dbl = fromRational diff_rat :: Float
      !amnt_dbl = fromIntegral numBytes :: Float
  writeConsole con $ "Read " ++ show numBytes ++ " in " ++ (show diff)
  writeConsole con $ "   " ++ showSpeed (amnt_dbl / diff_dbl) ++ "\n"
 where
  showSpeed x 
    | x < onek  = show x ++ " bps"
    | x < onem  = show (x / onek) ++ " KBps"
    | x < oneg  = show (x / onem) ++ " MBps"
    | otherwise = show (x / oneg) ++ " GBps"
  onek = 1024
  onem = 1024 * 1024
  oneg = 1024 * 1024 * 1024
