-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author:Adam Wick <awick@galois.com>,Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND


module Main where

import Testing.VM
import Dirty

import System.Environment
import Control.Monad
import Control.Applicative
import Data.Char


startPlayer :: String -> String -> Orc VM
startPlayer name config = 
  create player config [ VMExtra $ "player" ++ '=':name
                       , VMName player
                       ]
  where
    player = "Player" ++ name


test :: Bool -> Orc TestResult
test verbose = do
  [alice, bob, charlie] <- mapM (setup "Filter.config") ["A", "B", "C"]

  relay verbose alice   preamble
  relay verbose bob     preamble
  relay verbose charlie preamble

  r <- timeout testTimeout (Failure "Test script timed out") $
       pure Results <*> mapM (checkLine alice bob charlie) alice_script

  return $ summarize r

  where
    setup :: String -> String -> Orc VM
    setup config name = do
      vm <- startPlayer name config
      succeed $ interaction vm $ Series [anyLine, anyLine]
      return vm

    preamble :: Interaction
    preamble = Accept (Just "Setup concluded.") StdOut indefinitely

    alice_script :: [String]
    alice_script =
      concat $ replicate 20 $ [ "Woo == bad"
                              , "Evil wooness is bad"
                              , "But what about dirty evil that is bad -- what then?"
                              , "Surely, it too -- the evil -- is bad"
                              , "Well, maybe not"
                              ]

    checkLine :: VM -> VM -> VM -> String -> Orc TestResult
    checkLine alice bob charlie line = do
      when verbose $ report "=========================="
      -- Send the line to Alice's console
      succeed $ relay verbose alice $ Feed line
      when verbose $ report $ "Fed Alice: '" ++ line ++ "'"

      -- Bob outputs a regrade message for each word he redacts
      let naughty = filter isDirty $ words line
      when verbose $ report $ "Naughty words: " ++ show naughty

      bouts <-
        mapM ( \ bad -> relay verbose bob $
                        Expect bad StdOut verbalPause
             )
             naughty
      when verbose $ report "Waiting for Charlie ..."

      -- Charlie should report getting the redacted line
      cout <- relay verbose charlie $ Expect (redact line) StdOut verbalPause

      return $ Results $ bouts ++ [cout]


redact :: String -> String
redact =
  unwords .
  map (\ w -> if isDirty w then "X" else w ) .
  words

main :: IO ()
main = runTest test

