{-# LANGUAGE ScopedTypeVariables #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND

import Hypervisor.Kernel
import XenDevice.Console
import Hypervisor.Privileged(initIOPorts)
import Device.PCI.Probe
import Device.PCI.Device
import Device.PCI.ConfigSpace
import Control.Concurrent
import Device.PCI.BaseAddr
import Control.Monad

import Data.Bits
import Data.List (find)
import Data.Char (chr,ord)
import Data.Word
import Data.Traversable
import Foreign.Ptr (castPtr,plusPtr,Ptr)
import Foreign.Storable (Storable(poke,peek))
import Foreign.Marshal
import Hypervisor.Port (bindPhysicalIRQ)
import Hypervisor.Basics (DomId(..),Xen,Err(ENXIO),xTry)
import Hypervisor.Memory (mapForeignMachineFrames,toMFN,VPtr)
import Numeric

import BAR
import Capabilities
import PCIRegs
import Resources


-- Utilities -------------------------------------------------------------------

type Endo a = a -> a

showBits :: Integral a => a -> String
showBits b = showIntAtBase 2 (head . show) (fromIntegral b) ""

xenToIO :: Xen a -> IO (Maybe a)
xenToIO m = do
  res <- xTry m
  case res of
    Right a  -> return (Just a)
    Left err -> do
      writeStrLn ("xenToIO: " ++ show err)
      return Nothing


-- PCI -------------------------------------------------------------------------

-- | PCI Power States
data PowerState = D0 | D1 | D2 | D3hot | D3cold
  deriving (Eq,Show,Enum)

-- | Turn a power state into a masking function.
maskPowerState :: PowerState -> Endo Word16
maskPowerState st w = (w .&. complement 0x3) .|. pm
  where pm = fromIntegral (fromEnum st)

dumpMemory :: VPtr a -> Int -> IO ()
dumpMemory ptr num_pages = dumpMem' (castPtr ptr) (num_pages * 1024) where
  dumpMem' :: VPtr Word32 -> Int -> IO ()
  dumpMem' p 0 = return ()
  dumpMem' p n = do
    vs <- peekArray 8 p
    putStrLn $ foldr (\ x -> showHexPadded 8 x . showString " " ) "" vs
    dumpMem' (p `plusPtr` 32) (n-8)

showHexPadded :: (Integral a) => Int -> a -> ShowS
showHexPadded n x s =
  replicate (n - length hex) '0' ++ hex ++ s
  where hex = showHex x ""

-- See p. 388 of "PCI System Architecture" for details.  Pin values of
-- 1 through 4 correspond to the four PCI interrupt request pins, A# through
-- D#.  A value of 0 means the device doesn't support interrupts; all other
-- values are reserved.
showPin :: Word8 -> String
showPin n =
  case n of
    0          -> "<device doesn't use interrupts>"
    _ | n <= 4 -> [chr $ ord 'A' - 1 + (fromIntegral n), '#']
    _          -> "<reserved>"

main :: IO ()
main = halvm_kernel [dConsole] $ \_ -> do
  writeStrLn "Initializing IO ports:"
  b <- initIOPorts
  writeStrLn (if b then "Success" else "Failure")
  writeStrLn "Probing PCI bus:"
  tree <- probe
  drawTree writeStrLn (const Nothing, const Nothing) tree
  writeStrLn "Probe complete."
  writeStrLn "Exiting ..."
  threadDelay 2000000 -- wait for messages to escape VM event horizon

writeStrLn :: String -> IO ()
writeStrLn = writeConsole . (++ "\n")

