-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com> and Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Communication.IVC
import Data.ByteString.Lazy(ByteString)
import Data.Time
import Data.Word
import Hypervisor.Debug
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer
import XenDevice.Console
import XenDevice.Xenbus
import qualified Data.ByteString.Lazy as BS

import Common

main :: IO ()
main = halvm_kernel [dConsole,dXenbus] $ \ args -> do
  let args' = map killEqual $ map (break (== '=')) args
  case lookup "inchan" args' of
    Just inName -> do
      writeDebugConsole "END: Building input channel.\n"
      let (_, makeInChan) = sizedP2PConnection chanSize inName
      ichan::(InChannelEx Bin ByteString) <- makeInChan
      writeDebugConsole "END: Built input channel.\n"
      let items = pageReference ichan
      writeDebugConsole $ "END: Input channel has " ++ show (length items) ++ " items\n"
      getBinary ichan
      writeDebugConsole "END: Got first item.\n"
      start <- getCurrentTime
      writeDebugConsole "END: Got current time.\n"
      run_getter 0 0 start ichan
    Nothing -> do
      fail "END: Couldn't parse input channel!"
 where
  killEqual (a,'=':rest) = (a, rest)
  killEqual _            = error "No equal in second item!"

run_getter :: Word64 -> Word64 -> UTCTime -> InChannelEx Bin BS.ByteString ->  IO ()  
run_getter total x start_t c
  | x >= print_amt = do print_speed start_t total
                        run_getter total 0 start_t c
  | otherwise      = do block <- getBinary c
                        let size = fromIntegral $ BS.length block
                        run_getter (total + size) (x + size) start_t c
 where
  print_amt = 10 * 1024 * 1024

print_speed :: UTCTime -> Word64 -> IO ()
print_speed start numBytes = do
  now <- getCurrentTime
  let diff               = diffUTCTime now start
      diff_rat::Rational = toRational diff
      diff_dbl::Float    = fromRational diff_rat
      amnt_dbl::Float    = fromIntegral numBytes
  writeConsole $ "Read " ++ show numBytes ++ " in " ++ (show diff)
  writeConsole $ "   " ++ showSpeed (amnt_dbl / diff_dbl) ++ "\n"
 where
  showSpeed x 
    | x < onek  = show x ++ " bps"
    | x < onem  = show (x / onek) ++ " KBps"
    | x < oneg  = show (x / onem) ++ " MBps"
    | otherwise = show (x / oneg) ++ " GBps"
  onek = 1024
  onem = 1024 * 1024
  oneg = 1024 * 1024 * 1024
