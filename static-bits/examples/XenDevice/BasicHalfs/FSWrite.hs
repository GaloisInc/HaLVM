-- A basic example of creating and writing to a Halfs/HALVM file system
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick      <awick@galois.com>
--         Trevor Elliott <trevor@galois.com>
-- BANNEREND
--

-- Libraries
import Prelude hiding     ( putStr, putStrLn, print )

import Control.Concurrent.MVar (takeMVar)
import Halfs                   (StateHandle(..),mountFSMV,unmountFS,dHalfs)
import Halfs.Buffer            (withBuffSize)
import Hypervisor.Kernel       (halvm_kernel)
import XenDevice.Console       (dConsole)
import XenDevice.Disk          (initializeDisk)
import qualified HalfsIO

-- Friends
import Utils

runTest :: HalfsIO.WtHandle -> IO ()
runTest wh = withBuffSize 4096 $ \buf -> do
  putStrLn "Creating a couple files."
  writeFile wh buf 4096 "Hello, world\n" "/Header"
  HalfsIO.mkdir wh "/foo"
  writeFile wh buf 4096 "This is very important information!\n" "/foo/README"

runTest2 :: HalfsIO.RdHandle -> IO ()
runTest2 rh = do
  conts <- HalfsIO.getDirectoryContents rh "/"
  putStrLn ("Contents: " ++ show conts)

main = halvm_kernel [dConsole,dHalfs] $ \_ -> do
  putStrLn "Creating file system for disk hda1"
  initializeDisk "hda1"
  sh@(StateHandle mv _) <- mountFSMV Nothing "hda1" Nothing False 4096
  runTest (HalfsIO.WtHandle sh)
  putStrLn "Trying to read from it ..."
  runTest2 (HalfsIO.RdHandle sh)
  fsroot <- takeMVar mv
  unmountFS fsroot
  putStrLn "Done!"
  delay
