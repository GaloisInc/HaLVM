{-# LANGUAGE ScopedTypeVariables #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Control.Concurrent
import Control.Monad
import Data.Either
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Debug
import Communication.IVC
import Hypervisor.Kernel
import Hypervisor.Memory
import XenDevice.Xenbus
import Common
import Numeric

main = halvm_kernel [dXenbus] start
start _ = do
  c <- offer
  let dom = peer c
  refs <- forM [0..9] $ const allocRef
  writeDebugConsole $ "RCV: Generated " ++ show (length refs) ++ " refs.\n"
  badptr <- mallocBytes $ 4096 + (4096 * length refs)
  let ptr = alignPtr badptr 4096
  grantBufferAccess ptr dom refs
  fillBuffer ptr (4096 * length refs)
  put c refs
  writeDebugConsole $ "RCV: Starting to spin!\n"
  loop (ptr `plusPtr` (4096 * (length refs - 1)))
  writeDebugConsole $ "RCV: SUCCESS. (read second value.)\n"
  loop ptr
  writeDebugConsole $ "RCV: Succesfully read first value.\n"
 where
  loop ptr = do
    threadDelay (1 * 1000 * 1000)
    val :: Word32 <- peek ptr
    writeDebugConsole $ "RCV: Just read " ++ showHex val "\n"
    unless (val == 0x87654321) $ loop ptr

fillBuffer ptr 0 = return ()
fillBuffer ptr x = do
  poke ptr (0x12345678 :: Word32)
  fillBuffer (ptr `plusPtr` 4) (x - 4)

grantBufferAccess ptr dom [] = return ()
grantBufferAccess ptr dom (ref:rest) = do
  grantAccess ref dom ptr True
  grantBufferAccess (ptr `plusPtr` 4096) dom rest
