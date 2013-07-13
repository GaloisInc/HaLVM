-- A test program which implements the front-end of the "double device"
-- device driver. 
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.Word
import DoubleCommon
import Hypervisor.Kernel
import XenDevice.Console
import Communication.RingBuffer
import XenDevice.Xenbus

main = halvm_kernel [dConsole, dXenbus] main'

main' :: [String] -> IO ()
main' args = do
  let rounds = case args of
                 [arg] | take 7 arg == "rounds=" -> read $ drop 7 arg
                 _                               -> 100
 
  now <- getCurrentTime
  doubleLikeMad rounds 0 now 0 =<< client
  threadDelay 1000000 -- give messages time to reach outside the VM before shutdown

doubleLikeMad :: Int -> Word64 -> UTCTime -> Word64 -> DoubleFrontRing -> IO ()
doubleLikeMad 0      _count _start _num _frb = return ()
doubleLikeMad rounds  count  start  num  frb = do
  DoubleResponse _ doubleNum <- frbRequest frb (DoubleRequest num num)
  now <- getCurrentTime
  if diffUTCTime now start > 1
    then do
      putStrLn (show rounds ++ ": " ++ show count ++ " messages/second")
      doubleLikeMad (rounds - 1) 0           now   (num + 1) frb
    else do
      doubleLikeMad rounds       (count + 1) start (num + 1) frb

