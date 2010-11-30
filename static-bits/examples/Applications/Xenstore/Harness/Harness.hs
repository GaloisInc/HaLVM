
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
import Control.Applicative
import System.Environment


queries :: [String]
queries =
  [ "console"
  , "cpu"
  , "memory"
  , "store"
  , "bogus" -- not a real query
  , "quit"
  ]

responses :: [[String]]
responses =
  [ -- "console" should yield:
    [ "ring\\-ref = [0-9]+"
    , "port = [0-9]+"
    , "limit = [0-9]+"
    , "type = xenconsoled"      -- more possibilities?
    , "tty = /dev/pts/[0-9]+"
    ]

  , -- "cpu" should yield:
    [ "[0-9]:"
    , "availability = online"   -- more possibilities?
    ]

  , -- "memory" should yield:
    [ "target = [0-9]+" ]

  , -- "store" should yield:
    [ "ring\\-ref = [0-9]+"
    , "port = [0-9]+"
    ]

  , -- "bogus" should yield:
    [ "Error: ENOENT" ]

  , -- "quit" should yield:
    [ "Done." ]
  ]


test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "Xenstore"
  vm <- create testSubject (testSubject ++ ".config") []

  rs <- pure Results <*> zipWithM (inquire vm) queries responses
  
  return $ summarize rs

  where
    inquire :: VM -> String -> [String] -> Orc TestResult
    inquire vm query answers = do
      succeed $ interaction vm $ Feed query
      relay verbose vm $ acceptSeries answers


main :: IO ()
main = runTest test

