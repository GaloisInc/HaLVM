
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

import Testing.QEMU
import System.Environment
import Data.Char


test :: Bool -> Orc TestResult
test verbose = do
  let testSubject  = "ConsoleTest"
      disk         = "consoleTest.img"
      bootMsg      = "Booting HaLVM ... done."
  qm <- create testSubject $ stdQEMUArgs { qemu_disk = disk }

  boot <- relay verbose qm $ Accept (Just bootMsg) StdOut (Just $ 10 * seconds)
  rds <- relay verbose qm . Series . map testHyperCon =<< randomStrings
  destroy qm

  return $ summarize $ Results $ [ boot, rds ]

  where
    testHyperCon :: String -> Interaction
    testHyperCon inp = Series
      [ Feed inp
      , Accept (Just $ map toUpper inp) StdOut (Just $ 5 * seconds)
      ]


main :: IO ()
main = runTest test
