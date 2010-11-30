
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

import Testing.VM
import Control.Monad


test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "GetId"
      nugget      = "Got ID = DomId [0-9]+\\."
  vm <- create testSubject (testSubject ++ ".config") []

  liftM summarize $ relay verbose vm $ Accept (Just nugget) StdOut Nothing

main :: IO ()
main = runTest test
