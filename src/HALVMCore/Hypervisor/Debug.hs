-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |Debugging support for the HALVM.
--
-- This module currently only exports one routine, which allows you to
-- write out information on the Xen emergency console. It appears that
-- this console is only lightly buffered (it may be newline buffered, 
-- but we haven't experienced a emergency console flushing problem in
-- practice), and thus it's helpful in debugging.
--
-- The default Xen kernel does not support the emergency console in a
-- very useful way. It largely makes these writes into NOOPS. To see 
-- output, you need to make Xen using the "verbose=y" flag for both the
-- build *and* the install. Thus, build with "make verbose=y world" and
-- then install with "make verbose=y install".
module Hypervisor.Debug where

import Foreign.C.String
import Foreign.Ptr

-- |Write on the Xen emergency console.
writeDebugConsole :: String -> IO ()
writeDebugConsole str =
  withCString str $ \ cstr -> econsoleMsg (length str) cstr

foreign import ccall unsafe "econsole.h emergency_console_msg"
  econsoleMsg :: Int -> Ptr a -> IO ()
