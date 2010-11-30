
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

--import Orc
import Testing.VM hiding (testTimeout)

import Control.Monad
import Control.Applicative

import System.IO
import System.Environment
import Data.Char

testTimeout :: Int
testTimeout = 60 * seconds

server :: VM -> Bool -> Orc TestResult
server vm verbose = do
  boot <- relay verbose vm $ Series [ anyLine, anyLine ]
  when verbose $ report "Server alive"

  convo <- timeout testTimeout (Failure "Test failed: server timed out") $
           pure Results <*> 
           (mapM (relay verbose vm) $ replicate 200 anyLine)
  return $ summarize $ Results [boot, convo]


client :: VM -> Bool -> String -> String -> Orc TestResult
client vm verbose me you = do
  boot <- relay verbose vm $ Series [ anyLine, anyLine ]

  initial <- relay verbose vm $ Feed $ "Hello " ++ you

  convo <- timeout testTimeout (Failure "Test failed: client timed out") $
    pure Results <*>
    (mapM (relay verbose vm) $ replicate 100 $
       Series [ anyLine
              , Feed $ me ++ ": Hi again " ++ you
              ])

  return $ summarize $ Results [boot, initial, convo]


-- Run the chat: start the three "VM"s involved, passing in some
-- semaphores for synchronization.
chat :: Bool -> Orc TestResult
chat verbose = do
  let cfg   = "ChatDemo.config"
      andy  = "Andy"
      zebra = "Zebra"

  server_vm <- create "Server" cfg [ VMExtra "mode=server"
                                   , VMName "Server"
                                   ]
  andy_vm   <- create andy     cfg [ VMExtra "mode=client"
                                   , VMName andy
                                   ]
  zebra_vm  <- create zebra    cfg [ VMExtra "mode=client"
                                   , VMName zebra
                                   ]

  spawn [ server server_vm verbose
        , client andy_vm   verbose andy  zebra
        , client zebra_vm  verbose zebra andy
        ]


main :: IO ()
main = runTest chat

