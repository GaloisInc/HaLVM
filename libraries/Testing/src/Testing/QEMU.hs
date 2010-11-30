
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Testing.QEMU ( -- * Testing infrastructure
                      module Testing

                      -- * QEMU state type
                    , QEMU
                    , QEMUArgs(..)
              
                    -- * Exposed "QEMU" methods and attributes
                    , qemu_name            -- :: String
                    , qemu_putStr          -- :: String -> IO (Maybe String)
                    , qemu_putStrLn        -- :: String -> IO (Maybe String)
                    , qemu_getLn           -- :: IO String
                    , qemu_getErrLn        -- :: IO String
                    , qemu_verbose         -- :: Bool

                      -- * Basic "QEMU" operations
                    , create               -- :: String -> String -> [QEMUArg]
                                           -- -> Orc QEMU
                    , destroy              -- :: QEMU -> Orc ()

                      -- * Useful constants
                    , stdQEMUArgs          -- :: QEMUArgs
                    )
where

import Testing
import Testing.QEMU.Types

import Data.Maybe

-- | Launch a QEMU, yielding a "QEMU" state (containing methods for
-- communicating with the QEMU once it's created).
createQEMU :: String -> QEMUArgs -> Orc QEMU
createQEMU name qmargs = 
  ioOrc $ createSite name $ qemu : renderQEMUArgs qmargs

-- | Starts up the given "QEMU".
create :: String -> QEMUArgs -> Orc QEMU
create = createQEMU

-- | Shuts down the given "QEMU".
destroy :: QEMU -> Orc ()
destroy qm = do
  let qemuShutdownCode = "\SOHx" 
  interaction qm $ Feed qemuShutdownCode
  return ()

qemu_name     :: QEMU -> String
qemu_putStr   :: QEMU -> String -> IO (Maybe String)
qemu_putStrLn :: QEMU -> String -> IO (Maybe String)
qemu_getLn    :: QEMU -> IO String
qemu_getErrLn :: QEMU -> IO String
qemu_verbose  :: QEMU -> Bool

qemu_name      = site_name
qemu_putStr    = site_putStr
qemu_putStrLn  = site_putStrLn
qemu_getLn     = site_getLn
qemu_getErrLn  = site_getErrLn
qemu_verbose   = site_verbose

