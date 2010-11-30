
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


rounds :: Int
rounds = 100

anyLine_ :: Interaction
anyLine_ = Accept Nothing StdOut (Just $ 10 * seconds)

test :: Bool -> Orc TestResult
test verbose = do
  let doubler = "DoubleDevice"
      tester  = "DoubleTest"
      nugget  = "[0-9]+ messages/second"

  -- We don't interact with the DoubleDevice VM; DoubleTest does that using
  -- low-level facilities, and we watch DoubleTest.  Hence, we don't need to
  -- keep the VM handle for DoubleDevice.
  dvm <- create doubler (doubler ++ ".config") []
  drs <- relay verbose dvm $ Series [ anyLine_, anyLine_ ]

  vm <- create tester (tester ++ ".config")
               [VMExtra $ "rounds=" ++ show rounds]

  rs <- replicateM rounds $ relay verbose vm $
        Accept (Just nugget) StdOut Nothing

  return $ summarize $ Results $ drs : rs


main :: IO ()
main = runTest test

