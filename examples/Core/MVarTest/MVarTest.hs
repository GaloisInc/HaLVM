-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Control.Concurrent
import Hypervisor.Debug

main :: IO ()
main =
  do mv <- newEmptyMVar
     forkIO (otherThread mv)
     threadDelay 2000000
     writeDebugConsole "Waiting for other thread to be done.\n"
     takeMVar mv
     writeDebugConsole "Done. Should be some print-outs.\n"

otherThread :: MVar () -> IO ()
otherThread mv =
  do threadDelay 1000000
     writeDebugConsole "Here's a printout.\n"
     threadDelay 1000000
     writeDebugConsole "Here's a printout.\n"
     threadDelay 1000000
     writeDebugConsole "Here's a printout.\n"
     threadDelay 1000000
     putMVar mv ()


