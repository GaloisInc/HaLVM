-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import Control.Concurrent
import Control.Exception
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Debug
import Communication.IVC
import Hypervisor.Kernel
import Hypervisor.Memory
import XenDevice.Xenbus
import Common

main = halvm_kernel [dXenbus] start
start _ = do
  writeDebugConsole "SND: Accepting connection.\n"
  c <- accept
  writeDebugConsole "SND: Waiting for reference.\n"
  refs <- get c
  writeDebugConsole "SND: Mapping grants.\n"
  (ptr, _) <- mapGrants (peer c) refs True
  writeDebugConsole "SND: Filling buffer.\n"
  fillBuffer ptr (4096 * length refs)
  writeDebugConsole "SND: Successfully re-filled buffer.\n"
  threadDelay $ 5 * 1000 * 1000

fillBuffer ptr 0 = return ()
fillBuffer ptr x = do
  poke ptr (0x87654321 :: Word32)
  fillBuffer (ptr `plusPtr` 4) (x - 4)
