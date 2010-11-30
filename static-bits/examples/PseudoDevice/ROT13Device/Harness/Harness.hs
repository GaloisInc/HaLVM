
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
import System.Environment

import ROT13


rounds :: Int
rounds = 10

testTimeOut :: Maybe Int
testTimeOut =  Just $ 10 * seconds

test :: Bool -> Orc TestResult
test verbose = do
  let rot13er = "Backend"
      tester  = "Test"

  -- We don't interact with the DoubleDevice VM; DoubleTest does that using
  -- low-level facilities, and we watch DoubleTest.  Hence, we don't need to
  -- keep the VM handle for DoubleDevice.
  create rot13er (rot13er ++ ".config") []

  vm <- create tester (tester ++ ".config")
               [VMExtra $ "rounds=" ++ show rounds]

  succeed $ relay verbose vm $
            Accept (Just "Connected to server.") StdOut Nothing

  rs <- replicateM rounds $ cut $ testRound vm

  relay verbose vm $ Accept (Just "Done.") StdOut Nothing

  return $ summarize $ Results rs

  where
    open, close, stop :: String
    open  = "START ENCODED BLOCK"
    close = "END ENCODED BLOCK"
    stop  = "."

    testRound :: VM -> Orc TestResult
    testRound vm = do
      inputs <- randomStrings
      report $ unlines $ "Sending input:" : map ('\t':) inputs
      succeed $ interaction vm $ Series $ map Feed inputs
      succeed $ interaction vm $ Feed stop
      report "Waiting for response ..."
      succeed $ relay verbose vm $ Accept (Just open) StdOut testTimeOut
      report "Got it."
      match <- relay verbose vm $ Accept (Just close) StdOut Nothing

      return $ 
        case match of
          Success (Just (_, outputs))
            | map (map rot13encode) inputs == outputs -> match
            | otherwise                               -> Failure $ unlines
              [ "ROT13 incorrect; given inputs:"
              , unlines $ map ('\t':) inputs
              , "yields outputs:"
              , unlines $ map ('\t':) outputs
              ]
          _                                           -> match


main :: IO ()
main = runTest test
