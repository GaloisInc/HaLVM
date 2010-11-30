-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- Experimental child code demonstrating static IVC channels.

import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Debug(writeDebugConsole)
import System.Environment(getArgs)
import Communication.IVC(acceptChannel, put, get, unmarshall,
                     InChannelName, OutChannelName)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent(threadDelay)

main :: IO ()
main = halvm_kernel [] $ const $
  do writeDebugConsole $ "Hello started\n"
     c <- (unmarshall . BS.pack . head) `fmap` getArgs
     either runInputter runOutputter (c:: Either (InChannelName Int)
                                                 (OutChannelName Int))

runOutputter :: OutChannelName Int -> IO ()
runOutputter ocn =
  do writeDebugConsole "Running offerer\n"
     oc <- acceptChannel ocn
     threadDelay 1000000
     writeDebugConsole "Writing to channel\n"
     put oc 42
     writeDebugConsole "Offerer done\n"

runInputter :: InChannelName Int -> IO ()
runInputter icn =
  do writeDebugConsole "Running acceptor\n"
     ic <- acceptChannel icn
     writeDebugConsole "Reading from channel\n"
     i <- get ic
     writeDebugConsole $ "Acceptor got "++show i++"\n"
      



