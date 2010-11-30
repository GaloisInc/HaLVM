
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
import System.IO


count :: Int
count = 5

day, month, date, time, timezone, year :: String
day      = "(Mon|Tue|Wed|Thu|Fri|Sat|Sun)"
month    = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
date     = "[ ]?([0-9]+)"
time     = "([0-9][0-9]:[0-9][0-9]:[0-9][0-9])"
timezone = "HALVMST"
year     = "([0-9][0-9][0-9][0-9])"

test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "TimeTest"
      nugget      = unwords [ day, month, date, time, timezone, year ] 
  report "before the fork"
  vm <- create testSubject (testSubject ++ ".config")
               [VMExtra $ "count=" ++ show count]
  report "before rs"

  --rs <- relay verbose vm $ Series $ replicate count $
  rs <- relay verbose vm $ Series $ replicate count $
    Accept (Just nugget) StdOut Nothing
    -- FIXME: Ideally, we should also check that the date/times produced are
    -- in sequence, separated by one second.  Since we know the result matches
    -- the regex above, this could be achieved with some simple chopping and
    -- slicing (i.e., full parsing not necessary).
  report "after rs"

  return $ rs


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runTest test
