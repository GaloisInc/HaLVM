-- A basic example/test of reading from a Halfs/HALVM file system
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
import Prelude hiding          ( putStr, putStrLn, print )

import Control.Concurrent.MVar (takeMVar)
import Data.Integral           (INInt)
import Halfs                   (dHalfs,StateHandle(..),mountFSMV,unmountFS)
import Halfs.Buffer            (Buffer,withBuffSize)
import Hypervisor.Kernel       (halvm_kernel,halvm_shutdown)
import System.FilePath         ((</>))
import XenDevice.Console       (dConsole)
import XenDevice.Disk          (initializeDisk)
import qualified HalfsIO

-- Friends
import Utils

type Indent = String

runFind :: HalfsIO.RdHandle -> String -> Indent -> String -> IO ()
runFind _  _ _ "."  = return ()
runFind _  _ _ ".." = return ()
runFind rh base indent relpath = do
  let path = base </> relpath
  dir <- HalfsIO.isDirectory rh path
  if dir
    then do
      putStrLn (indent ++ relpath ++ "/")
      mapM_ (runFind rh path (indent ++ "  "))
        =<< HalfsIO.getDirectoryContents rh path
    else putStrLn (indent ++ relpath)

dumpFile :: HalfsIO.RdHandle -> Buffer -> INInt -> FilePath -> IO ()
dumpFile rh buf len path = do
  putStrLn ""
  header path
  putStrLn =<< readFile rh buf len path

runTest :: HalfsIO.RdHandle -> IO ()
runTest rh = withBuffSize 4096 $ \buf -> do
  contents <- HalfsIO.getDirectoryContents rh "/"
  header "File structure:"
  mapM_ (runFind rh "/" "") contents
  dumpFile rh buf 4096 "/Header"
  dumpFile rh buf 4096 "/foo/README"

main = halvm_kernel [dConsole, dHalfs] $ \_ -> do
  initializeDisk "hda1"
  putStrLn "Starting file system operations."
  sh@(StateHandle mv _) <- mountFSMV Nothing "hda1" Nothing False 4096
  runTest (HalfsIO.RdHandle sh)
  unmountFS =<< takeMVar mv
  putStrLn "Done!"
  delay
  halvm_shutdown
