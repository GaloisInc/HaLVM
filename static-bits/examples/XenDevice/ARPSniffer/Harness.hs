
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


hex :: String
hex = "[0-9a-f]+"

mac :: String
mac = foldr1 ( \ hh pat -> hh ++ ':' : pat ) $ replicate 6 hex

ipnode :: String
ipnode = "[0-9]+"

ip :: String
ip = foldr1 ( \ node pat -> node ++ "\\." ++ pat ) $ replicate 4 ipnode

request :: String
request = unwords 
  [ mac
  , "\\->"
  , mac ++ ":"
  , ip
  , "REQUESTs"
  , ip
  ]

response :: String
response = unwords
  [ mac
  , "\\->"
  , mac ++ ":"
  , ip
  , "RESPONDs"
  , mac
  , "for"
  , ip
  ]

unknown :: String
unknown = unwords
  [ mac
  , "\\->"
  , mac ++ ":"
  , "UNKNOWN TYPE \\(" ++ hex ++ "\\)"
  ]

rounds :: Int
rounds = 10

sniffTimeout :: Maybe Int
sniffTimeout = Just $ 30 * seconds

test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "ARPSniffer"
      legal       = concat [ "(", request, "|", response, "|", unknown, ")" ]
  vm <- create testSubject (testSubject ++ ".config") []

  rs <- replicateM rounds $ relay verbose vm $
        Accept (Just legal) StdOut Nothing -- sniffTimeout

  return $ summarize $ Results rs

main :: IO ()
main = runTest test
