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
import Communication.IVC
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer
import XenDevice.Console
import XenDevice.Xenbus

accept :: IO (OutChannel String)
(_, accept) = p2pConnection "c_logger"

main :: IO ()
main = halvm_kernel_daemon [dXenbus,dConsole] $ \ _ -> do
  chan <- accept
  writeConsole "Connected.\n"
  echo chan "Interesting event #1.\n"
  threadDelay 1000000
  echo chan "Interesting event #2, with more info.\n"
  threadDelay 1000000
  echo chan "Final event.\n"
  forever $ put chan =<< getLnConsole
  where
    echo :: OutChannel String -> String -> IO ()
    echo c s = do
      writeConsole s
      put c s

