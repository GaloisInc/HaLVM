-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
-- |A XenDevice for the Xen hypervisor console, that is, the console
-- which is accessed through the HYPERVISOR_console_io hypercall.  For
-- output, one can more easily use the function
-- 'Hypervisor.Debug.writeDebugConsole'.  However, if a domain needs
-- input from the hypervisor console, this module can be used.  Note
-- that it is a bad idea to have more than one domain do
-- hypervisor-console input.  Normally, linux-2.6-xen0 does just that,
-- so this module is mostly useful in worlds without the Grand Ole
-- Domain Zero (GODZ).
--
-- Note the difference between this module and "XenDevice.Console".
-- That module provides an interface to the ring-buffer console
-- connection to Domain Zero.

module XenDevice.HypervisorConsole(
  dHypervisorConsole, readHypervisorConsole, getLnHypervisorConsole, writeHypervisorConsole)
  where

import Control.Concurrent.Chan(Chan,newChan,writeChan,readChan)
import Control.Monad(when)
import Foreign.C.String(peekCStringLen,CString)
import Foreign.Marshal.Array(allocaArray)
import GHC.IOBase(unsafePerformIO)
import Hypervisor.Debug (writeDebugConsole)
import Hypervisor.Kernel(DeviceDriver(..))
import Hypervisor.Port(bindVirq,setPortHandler)
import Hypervisor.Virqs(virqCONSOLE)
import qualified XenDevice.ConsoleAux as Aux

-- |The definition of the HypervisorConsole device driver. If you intend to use any of the
-- other exported functions, you should include this in the list of drivers passed
-- to halvm_kernel or halvm_kernel_daemon.
dHypervisorConsole :: DeviceDriver
dHypervisorConsole = DeviceDriver { devName = "XenDeviceInternalHypervisorConsole"
                        , dependencies = []
                        , initialize = initHypervisorConsole
                        , shutdown = return ()
                        }

initHypervisorConsole :: IO ()
initHypervisorConsole = 
    do vp <- bindVirq virqCONSOLE 0
       setPortHandler vp consoleSignal

consoleSignal :: IO ()
consoleSignal =
  do let bufSize = 100
     len <- allocaArray bufSize
             (\a -> do len <- emergency_console_read a bufSize
                       peekCStringLen (a,len) >>= mapM_ (writeChan consoleReaderChan)
                       return len)
     when (len > 0) consoleSignal



-- |Read the given number of bytes from the console, returning them as a string.
-- This routine will block until the given number of bytes are available.
readHypervisorConsole :: Int -> IO String
readHypervisorConsole n = Aux.xlateInput `fmap` 
                          sequence (replicate n (readChan consoleReaderChan))

-- | Read a line from the console, echoing the characters.
getLnHypervisorConsole :: IO String
getLnHypervisorConsole = Aux.getLnConsole readHypervisorConsole 
                                          writeHypervisorConsole

-- |Write the given string to the console.
writeHypervisorConsole :: String -> IO ()
writeHypervisorConsole = writeDebugConsole

{-# NOINLINE consoleReaderChan #-}
consoleReaderChan :: Chan Char
consoleReaderChan = unsafePerformIO newChan

foreign import ccall unsafe "econsole.h emergency_console_read" emergency_console_read :: CString -> Int -> IO Int
