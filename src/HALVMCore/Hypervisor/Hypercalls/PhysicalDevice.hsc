-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
module Hypervisor.Hypercalls.PhysicalDevice where

import Control.Exception
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Hypervisor.ErrorCodes

#include <stdint.h>
#define __XEN_TOOLS__
#include <xen/domctl.h>
#include <xen/sysctl.h>
#include <xen/physdev.h>

data PhysicalDeviceOp = PhysDevOpEOI
                      | PhysDevOpSetIOPrivLevel

pdCmdVal :: PhysicalDeviceOp -> Int
pdCmdVal PhysDevOpEOI            = (#const PHYSDEVOP_eoi)
pdCmdVal PhysDevOpSetIOPrivLevel = (#const PHYSDEVOP_set_iopl)

pdCmdSize :: PhysicalDeviceOp -> Int
pdCmdSize PhysDevOpEOI            = (#size physdev_eoi_t)
pdCmdSize PhysDevOpSetIOPrivLevel = (#size physdev_set_iopl_t)

physicalDeviceOp :: PhysicalDeviceOp ->
                    (Ptr a -> IO b)      ->
                    (b -> Ptr a -> IO c) ->
                    IO c
physicalDeviceOp cmd setter getter =
  bracket (mallocBytes (pdCmdSize cmd)) free $ \ buffer -> do
    bzero buffer (fromIntegral (pdCmdSize cmd))
    setres  <- setter buffer
    initres <- do_physdev_op (pdCmdVal cmd) buffer
    if initres == 0
      then getter setres buffer
      else throw (toEnum (-initres) :: ErrorCode)

foreign import ccall unsafe "strings.h bzero"
  bzero :: Ptr a -> Word -> IO ()

foreign import ccall unsafe "hypercalls.h HYPERCALL_physdev_op"
  do_physdev_op :: Int -> Ptr a -> IO Int

