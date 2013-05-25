-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |This module defines the wrappers for writing HALVM kernels. In short, to 
-- make a HALVM virtual machine, write a normal Haskell program. Then rename
-- your "main" function to something else, like "start". Then simply add the
-- following line to your file: "main = halvm_kernel start". 
module Hypervisor.Kernel(DeviceDriver(..), 
                         halvm_kernel,
                         halvm_kernel_daemon,
                         halvm_shutdown) 
    where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO.Unsafe(unsafePerformIO)
import Hypervisor.Debug(writeDebugConsole)
import System.Environment
import System.Exit
import System.IO (setXenPutStr)

-- |These types define the functions associated with device drivers. They 
-- should be straightforward. Note that it's important to remember the 
-- one-to-many relationship between device drivers and devices. There is one
-- device driver for disks, even though there may be many disks.
data DeviceDriver = DeviceDriver { 
    devName :: String -- ^ The name of the device driver.
  , dependencies :: [DeviceDriver] -- ^ Drivers this driver depends on.
  , initialize :: IO () -- ^ A computation that will initialize the device(s)
  , shutdown :: IO () -- ^ A computation that will shut down the device(s)
  }

-- This code generates a list for initializing device drivers. Note that the 
-- list we're given may be incomplete, so it's important to check for 
-- additional requirements in the list. Also note that the length of the 
-- output list will be >= the length of the input list, not strictly equal to.
generateDDriverInitOrdering :: [DeviceDriver] -> [DeviceDriver]
generateDDriverInitOrdering inDevs =
    let allDevs = fetchAllDevs [] inDevs
    in generateOrder allDevs

fetchAllDevs :: [String] -> [DeviceDriver] -> 
                [(String,DeviceDriver,[DeviceDriver])]
fetchAllDevs _ [] = []
fetchAllDevs doneNames (first:rest) 
  | (devName first) `elem` doneNames = fetchAllDevs doneNames rest
  | otherwise =
    let deps = dependencies first
        dname = devName first
    in (dname,first,deps):(fetchAllDevs (dname:doneNames) (rest ++ deps))

generateOrder :: [(String, DeviceDriver, [DeviceDriver])] -> [DeviceDriver]
generateOrder [] = []
generateOrder devs =
  if null firstNameDevs
     then error $ "Cyclic dependency in devices? (" ++ 
                  (show (map (\(x,_,_) -> x) devs)) ++ ")"
     else firstDevs ++ (generateOrder rest)
 where
  firstNameDevs = [(name, dev) | (name, dev, []) <- devs]
  firstNames = [name | (name,_) <- firstNameDevs]
  firstDevs = [dev | (_,dev) <- firstNameDevs]
  rest = [(name,dev,deps') | (name, dev, deps) <- devs,
                             not $ name `elem` firstNames,
                             let deps' = [dep | dep <- deps, 
                                                not $ (devName dep) `elem` 
                                                      firstNames]]
                                
-- |A wrapper for HALVM kernels. It sets up the various devices and subsystems 
-- required for a HALVM kernel to run, and then invokes the given function 
-- with the arguments in the domain config file. Note that the wrapper will 
-- immediately begin the shutdown sequence once the passed function returns 
-- a value; it will not wait for all threads to halt. If you would prefer the 
-- VM to never shutdown, or be shutdown only through an explicit call to 
-- halvm_shutdown, then use halvm_kernel_daemon.
halvm_kernel :: [DeviceDriver] -> ([String] -> IO ()) -> IO ()
halvm_kernel ddrivers fun = run_kernel ddrivers fun halvm_shutdown

-- |Like halvm_kernel, but will never return.
halvm_kernel_daemon :: [DeviceDriver] -> ([String] -> IO ()) -> IO ()
halvm_kernel_daemon ddrivers fun = run_kernel ddrivers fun uffish_thought

-- |Causes an immediate shutdown of the HALVM. Note that unlike 
-- System.Exit.exitWith, this will throw no exceptions, so if you want to do
-- any clean-ups in other threads this won't work for you.
halvm_shutdown :: IO ()
halvm_shutdown = do ddrivers <- takeMVar deviceDrivers
                    sequence $ map shutdown (reverse ddrivers)
                    writeDebugConsole "Immediate exit by halvm_shutdown.\n"
                    do_exit

deviceDrivers :: MVar [DeviceDriver]
deviceDrivers = unsafePerformIO $ newMVar []

-- if you don't get this, you need to read more Lewis Carroll
uffish_thought :: IO ()
uffish_thought = threadDelay 10000000 >> uffish_thought

run_kernel :: [DeviceDriver] -> ([String] -> IO ()) -> IO () -> IO ()
run_kernel ddrivers first after = catches startupMachine [
    Handler exitCodeHandler
  , Handler otherHandler
  ]
 where
  exitCodeHandler :: ExitCode -> IO ()
  exitCodeHandler ExitSuccess     = return ()
  exitCodeHandler (ExitFailure x) = 
    writeDebugConsole $ "Domain exited with error code: " ++ show x ++ "\n"
  --
  otherHandler :: SomeException -> IO ()
  otherHandler se =
    writeDebugConsole $ "Uncaught exception: " ++ show se ++ "\n"
  --
  startupMachine = do
    setXenPutStr writeDebugConsole
    takeMVar deviceDrivers
    let orderedDeviceDrivers = generateDDriverInitOrdering ddrivers
    sequence $ map initialize orderedDeviceDrivers
    putMVar deviceDrivers orderedDeviceDrivers
    main0 first
    after

main0 :: ([String] -> IO ()) -> IO ()
main0 body =
    do args <- liftM (words.concat) getArgs
       body args

foreign import ccall unsafe "hbmxen.h do_exit"
  do_exit :: IO ()

