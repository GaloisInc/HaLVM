
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

import Testing.VM


test :: Bool -> Orc TestResult
test _ = do
  let testSubject = "ExitTest"
      shutdownMsg = "Shutting down now\\!"
  vm <- create testSubject (testSubject ++ ".config") []

  interaction vm $ Accept (Just shutdownMsg) StdOut Nothing

main :: IO ()
main = runTest test

