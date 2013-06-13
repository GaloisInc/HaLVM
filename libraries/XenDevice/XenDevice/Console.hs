-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- |Routines for dealing with the Xen console. Remember that in order to 
-- interact with the console you must either start the domain with the "-c" 
-- flag to "xm create", or run "xm console" as a seperate command.
--
-- IMPORTANT NOTE: The Xen console is heavily buffered, so is probably not a
-- good choice for reliable debugging. For that, see the Hypervisor.Debug module
-- in the HALVMCore libraries.
module XenDevice.Console(
    dConsole
  , mkConsoleChannels
  , readConsole
  , getLnConsole
  , writeConsole
  ) where

import Control.Concurrent(threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Hypervisor.Basics
import Hypervisor.BufferChan
import Hypervisor.Kernel
import Hypervisor.Memory
import Hypervisor.Port
import qualified XenDevice.ConsoleAux as Aux
import qualified XenDevice.ConsoleInterface as I
import XenDevice.ConsoleInterface(Interface(..),IdxPtr)
import Foreign.C.Types(CChar)
import System.IO (setXenPutStr,setXenGetChar)
import System.IO.Unsafe (unsafePerformIO)

import Hypervisor.Debug

-- |The definition of the Console device driver. If you intent to use any of the
-- other exported functions, you should include this in the list of drivers passed
-- to halvm_kernel or halvm_kernel_daemon.
dConsole :: DeviceDriver
dConsole = DeviceDriver { devName = "XenDeviceInternalConsole"
                        , dependencies = []
                        , initialize = initConsole
                        , shutdown = return ()
                        }

initConsole :: IO ()
initConsole =
    do intMFN <- I.getConsoleMFN
       intf   <- mfnToVPtr intMFN `xCatch` (\ _ -> do
                   mapForeignMachineFrames domidSelf [intMFN])
       consolePort <- I.getConsolePort
       i <- I.interface intf consolePort
       (r, w) <- mkConsoleChannels i
       putMVar consoleReaderChan r
       putMVar consoleWriterChan w
       threadDelay 750000 -- Delay seems to be necessary for Xen's console
                          -- to catch up to us.
       setXenPutStr writeConsole
       setXenGetChar (head `fmap` readConsole 1)

-- |Given a console interace, create console channels for reading and
-- writing, respectively. This should be used rarely, but is occasionally
-- useful when writing your own console drivers.
mkConsoleChannels :: I.Interface -> IO (BoundedChan Char, BoundedChan String)
mkConsoleChannels i = do
  r <- mkBoundedReaderChan 200
        (readConsole' (in_prodp i) (in_consp i) (in_shdata i) 
                      (interface_port i) (in_ring_size i))
        (I.canReadConsole (in_prodp i) (in_consp i))
        (waitSet i) 
        (interface_port i) 
  w <- mkBoundedWriterChan 200 
        (writeConsole' (out_prodp i) (out_consp i) (out_shdata i)
                       (interface_port i) (out_ring_size i))
        (I.canWriteConsole (out_prodp i) (out_consp i) (out_ring_size i)) 
        (waitSet i) 
        (interface_port i)
  return (r, w)


-- |Read the given number of bytes from the console, returning them as a string.
-- This routine will block until the given number of bytes are available.
readConsole :: Int -> IO String
readConsole n = 
   do chan <- readMVar consoleReaderChan	   
      sequence (replicate n (readChan chan))

-- | Read a line from the console, echoing the characters.
getLnConsole :: IO String
getLnConsole = Aux.getLnConsole readConsole writeConsole

-- |Write the given string to the console. This may block until the string has
-- been written to the console back-end, but does not block until the string has
-- been flushed to the console.
writeConsole :: String -> IO ()
writeConsole s = 
   do chan <- readMVar consoleWriterChan
      writeChan chan (Aux.xlateOutput s)

{-# NOINLINE consoleReaderChan #-}
consoleReaderChan :: MVar (BoundedChan Char)
consoleReaderChan = unsafePerformIO newEmptyMVar

{-# NOINLINE consoleWriterChan #-}
consoleWriterChan :: MVar (BoundedChan String)
consoleWriterChan = unsafePerformIO newEmptyMVar
  
writeConsole' :: IdxPtr -> IdxPtr -> Ptr CChar -> Port -> Int -> String -> IO Int
writeConsole' prodp consp shdata port bufsize s =  
  withCStringLen s $ \(a,l) -> I.writeConsole prodp consp shdata port bufsize a l

readConsole' :: IdxPtr -> IdxPtr -> Ptr CChar -> Port -> Int -> IO String
readConsole' prodp consp shdata port bufsize = 
  allocaArray bufsize
  (\a -> do len <- I.readConsole prodp consp shdata port bufsize a bufsize
	    s <- peekCStringLen (a,len)
	    return $ Aux.xlateInput s)
