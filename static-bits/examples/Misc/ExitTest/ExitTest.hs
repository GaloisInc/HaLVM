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
import Hypervisor.Kernel
import XenDevice.Console

main :: IO ()
main = halvm_kernel [dConsole] $ \ _ -> do
  forkIO exitThread
  forever $ threadDelay 1000000

exitThread :: IO ()
exitThread = do
  threadDelay 1000000
  writeConsole "Shutting down now!\n"
  threadDelay 1000
  halvm_shutdown

