
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

{- |

  Module      : Testing.QEMU.Types
  Copyright   : (c) 2009 Galois, Inc.

  Defines a qemu virtual machine state and methods for interacting with it,
  via specialized console-based I/O functions.

-}

module Testing.QEMU.Types
  ( -- * Types
    -- ** Virtual Machine Parameters
    QEMU
  , QEMUArgs(..)

    -- * Auxiliary functions
  , renderQEMUArgs       -- :: QEMUArgs -> String

    -- * Useful constants
  , stdQEMUArgs          -- :: QEMUArgs
  , defaultQEMUDisk      -- :: String
  , defaultQEMUMem       -- :: Int
  , qemu                 -- :: String
  )
where

import Testing.Site


type QEMU = Site

-- | Models a subset of @xm create@'s parameters, namely @-c@, @name=@, and
-- @extra=@.
data QEMUArgs
  = QEMUArgs { qemu_mem    :: Int
             , qemu_disk   :: String
             , qemu_serial :: Maybe String
             , qemu_base   :: [String]
             }
    deriving ( Eq, Show )

-- | A simple pretty printer for "QEMUArgs", intended to be formatted as
-- appropriate for being passed to `qemu`.
renderQEMUArgs :: QEMUArgs -> [String]
renderQEMUArgs (QEMUArgs mem disk mserial base) = 
  base
  ++
  [ "-hda", disk
  , "-m", show mem
  ]
  ++
  maybe [] (\ h -> [ "-serial", h ]) mserial


-- | Standard set of arguments for launching a "QEMU" site.
stdQEMUArgs :: QEMUArgs
stdQEMUArgs =
  QEMUArgs { qemu_mem    = defaultQEMUMem
           , qemu_disk   = defaultQEMUDisk
           , qemu_serial = Nothing
           , qemu_base   = [ "-nographic", "-no-kqemu", "-no-reboot" ]
           }

-- | Default memory size.
defaultQEMUMem :: Int
defaultQEMUMem= 256

-- | Default disk image name.
defaultQEMUDisk :: String
defaultQEMUDisk= "disk.img"

-- | Path to `qemu` executable.
qemu :: String
qemu = "/usr/bin/qemu"

