
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Testing.VM ( -- * Testing infrastructure
                    module Testing

                    -- * VM state type
                  , VM
                  , VMArg(..)
              
                  -- * Exposed "VM" methods and attributes (renamed versions of
                  -- generic "Site" methods
                  , vm_name            -- :: String
                  , vm_putStr          -- :: String -> IO (Maybe String)
                  , vm_putStrLn        -- :: String -> IO (Maybe String)
                  , vm_getLn           -- :: IO String
                  , vm_getErrLn        -- :: IO String
                  , vm_verbose         -- :: Bool

                    -- * Basic "VM" operations
                  , create             -- :: String -> Maybe (MVar ()) -> String
                                       -- -> [VMArg] -> Orc VM
                  , create_            -- :: String -> Maybe (MVar ()) -> String
                                       -- -> [VMArg] -> Orc VM
                  , destroy            -- :: VM -> Orc ()
                  )
where

import Testing
import Testing.VM.Types

import Control.Monad
import Data.Maybe

import System.Process


-- | Launch a VM, yielding a "VM" state (containing methods for communicating
-- with the VM once it's created).
createVM :: String -> String -> [VMArg] -> Orc VM
createVM name config vmargs =
  ioOrc $ createSite name $ [ xm, "create", config ] ++ map renderVMArg vmargs

-- | Launch a VM, yielding a "VM" state (containing methods for communicating
-- with the VM once it's created), but don't attach a console.
create_ :: String -> String -> [VMArg] -> Orc VM
create_ = createVM

-- | Launch a VM, yielding a "VM" state (containing methods for communicating
-- with the VM once it's created).
create :: String -> String -> [VMArg] -> Orc VM
create name config vmargs =
  createVM name config $ VMConsole : vmargs

-- | Shuts down the given "VM".
destroy :: VM -> Orc ()
destroy vm = 
  ioOrc (mustSucceed =<< (liftM Just $ rawSystem xm [ "destroy", vm_name vm ]))


vm_name     :: VM -> String
vm_putStr   :: VM -> String -> IO (Maybe String)
vm_putStrLn :: VM -> String -> IO (Maybe String)
vm_getLn    :: VM -> IO String
vm_getErrLn :: VM -> IO String
vm_verbose  :: VM -> Bool

vm_name      = site_name
vm_putStr    = site_putStr
vm_putStrLn  = site_putStrLn
vm_getLn     = site_getLn
vm_getErrLn  = site_getErrLn
vm_verbose   = site_verbose

