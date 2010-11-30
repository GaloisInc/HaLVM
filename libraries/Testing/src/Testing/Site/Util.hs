
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Testing.Site.Util
  ( -- * Constants
    sudo         -- :: FilePath
  
    -- * Handle utilities
  , textify      -- :: Handle -> IO ()

    -- * Reifying errors
  , mustSucceed  -- :: Maybe ExitCode -> IO ()
  )
where

import System.IO
import System.Exit


-- | Path to `sudo` executable.
sudo :: FilePath
sudo = "/usr/bin/sudo"


-- | Sets the given handle to be in text mode.
textify :: Handle -> IO ()
textify h = do
  hSetBuffering  h NoBuffering
  hSetBinaryMode h False


-- | Given a result value and maybe an "ExitCode" from a forked call to @xm@
-- or @qemu@, returns the result unless the "ExitCode" denotes definite
-- failure.  "Nothing" is interpetered as mustSucceed, indicating that the
-- process is still in the O/S's process table.  This is not necessarily a
-- success (it's possible that the @xm@/@qemu@ call could still fail), it
-- probably is (if @xm@/@qemu@ fails, it generally does so pretty quickly).
mustSucceed :: Maybe ExitCode -> IO ()
mustSucceed Nothing                = return ()
mustSucceed (Just ExitSuccess)     = return ()
mustSucceed (Just e@ExitFailure{}) = 
  error $ "Testing.Site: low-level site failure: " ++ show e

