{-# LANGUAGE PatternGuards #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND


import Hypervisor.Kernel(halvm_kernel)
import Hypervisor.Privileged(initIOPorts)
import Hypervisor.Memory(allocPage,VPtr)
import XenDevice.HypervisorConsole

import Device.PCI.Probe(probe)
import Device.PCI.Device(findDev)

import Foreign.Ptr
import Data.Char
import Data.Word
import Data.List

import Device.IDE.Block
import Device.IDE.Info
import Device.IDE.API
import Device.IDE.Debug

main :: IO ()
main = halvm_kernel [dHypervisorConsole] $ \_ ->
  do initIOPorts
     tree <- probe
     case findDev 0x8086 0x7010 tree of
       Nothing  -> writeStrLn "Cannot find device."
       Just dev -> do
         ds <- get_devices dev
         test (names `zip` ds)
           `catch`
           (logError "An exception occured:")
  where
    names = [ "hd" ++ [x] | x <- ['a'..] ]

test :: [(String, Device)] -> IO ()
test ds = do
  mp <- allocPage
  case mp of
    Right n -> loop Nothing (castPtr n)
    Left _  -> writeStrLn "Not enough memory to allocate buffer."

  where
    with_dev Nothing      p _f = do
      writeStrLn "No device is selected."
      loop Nothing p
    with_dev (Just (_,x)) _ f  = f x

    show_selected Nothing       = "(none)"
    show_selected (Just (x, _)) = x

    loop d p  = do
      writeStr ("device " ++ show_selected d ++ "> ")
      xs <- readLn
      case words xs of
        ["ls"] -> do
          mapM (writeStrLn . fst) ds
          loop d p

        ["select", a] -> do
          case lookup a ds of
            Just dev -> do
              select_device (resources dev) (number dev)
              loop (Just (a,dev)) p
            Nothing  -> do
              writeStrLn "Unknown device."
              loop d p

        ["info"] -> with_dev d p $ \dev -> do
          i <- identify_device (resources dev)
          writeStrLn $ unlines $ pp_info $ parse_device_info i
          loop d p

        ["copy", a, b] | Just from <- readMaybe a, Just to <- readMaybe b ->
          with_dev d p $ \dev -> do
          copy dev p from to
          loop d p

        [w] | Just r <- readMaybe w ->
          with_dev d p $ \dev -> do
          merr <- read_block dev r p
          maybe (dump_read r) (logError "ERROR READING:") merr
          loop d p
          where
            dump_read r = dump writeStrLn (r * 4096) 4096 (castPtr p)

        ["help"] -> do
          writeStrLn $ unlines
            [ "ls                  -- list available devices"
            , "select <device>     -- selects a device"
            , "info                -- display information about device"
            , "copy <from> <to>    -- copy a block to another"
            , "<block>             -- display the contents of a block"
            , "help                -- this help"
            ]
          loop d p

        _ -> do
          writeStrLn "Invalid command, try 'help'."
          loop d p

readMaybe :: Read a => String -> Maybe a
readMaybe x =
  case reads x of
    [(a,as)] | all isSpace as -> Just a
    _                         -> Nothing

copy :: Device -> VPtr a -> Word32 -> Word32 -> IO ()
copy r buff from to = do
  merr <- read_block r from buff
  maybe do_write (logError "ERROR READING:") merr
  where
    do_write :: IO ()
    do_write = do
      merr <- write_block r to buff
      maybe (return ()) (logError "ERROR WRITING:") merr


--------------------------------------------------------------------------------
writeStr, writeStrLn :: String -> IO ()
writeStr   = writeHypervisorConsole
writeStrLn = writeStr . (++ "\n")

write :: Show a => a -> IO ()
write      = writeStrLn . show

readLn :: IO String
readLn     = getLnHypervisorConsole

logError :: Show a => String -> a -> IO ()
logError msg err = do
  writeStrLn msg
  write err

