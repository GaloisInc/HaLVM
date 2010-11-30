
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

{- |

  Module      : Testing.VM.Types
  Copyright   : (c) 2009 Galois, Inc.

  Defines a virtual machine state and methods for interacting with it,
  via specialized console-based I/O functions.

-}

module Testing.VM.Types
  ( -- * Types
    -- ** Virtual Machine Parameters
    VM
  , VMArg(..)

    -- * Auxiliary functions
  , renderVMArg        -- :: VMArg -> String
  , xm                 -- :: FilePath
  )
where

import Testing.Site

import System.IO


type VM = Site

-- | Models a subset of @xm create@'s parameters, namely @-c@, @name=@, and
-- @extra=@.
data VMArg
  = VMConsole
  | VMExtra String
  | VMName String
    deriving ( Eq, Show )

-- | A simple pretty printer for "VMArg", intended to be formatted as
-- appropriate for being passed to @xm create@.
renderVMArg :: VMArg -> String
renderVMArg VMConsole       = "-c"
renderVMArg (VMExtra extra) = "extra=" ++ extra
renderVMArg (VMName name)   = "name=" ++ name

-- | Path to `xm` executable.
xm :: FilePath
xm = "/usr/sbin/xm"

