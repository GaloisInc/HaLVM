
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


hex :: String
hex = "[0-9a-f]+"

mac :: String
mac = foldr1 ( \ hh pat -> hh ++ ':' : pat ) $ replicate 6 hex

myMAC :: String
myMAC = "My MAC address is "

getMAC :: TestResult -> Orc String
getMAC r =
  case r of
    Success (Just (line, _)) -> return $ drop (length myMAC) line
    _                        -> do
      report $ unlines [ "Not a legal 'My MAC is ...' line; got:"
                       , '\t':show r
                       ]
      mzero

rounds :: Int
rounds = 10

getMACTimeout :: Maybe Int
getMACTimeout = Just $ 10 * seconds

test :: Bool -> Orc TestResult
test verbose = do
  let pingerName = "Pinger"
      serverName = "Server"
      acceptMAC  = Accept (Just $ myMAC ++ mac) StdOut getMACTimeout

  server <- create serverName (serverName ++ ".config") []
  pinger <- create pingerName (pingerName ++ ".config") 
                              [ VMExtra $ "rounds=" ++ show rounds ]

  rtimer $ 3 * seconds

  when verbose $ report "Getting Server's MAC"
  servMAC <- getMAC =<< relay verbose server acceptMAC
  when verbose $ report ('\t' : show servMAC)

  when verbose $ report "Getting Pinger's MAC"
  pingMAC <- getMAC =<< relay verbose pinger acceptMAC
  when verbose $ report ('\t' : show pingMAC)

  when verbose $ report $ "Testing ping, rounds trips = " ++ show rounds
  rs <- forM [1 .. rounds] $ roundTripPing pinger server pingMAC servMAC

  return $ summarize $ Results rs

  where
    roundTripPing :: VM -> VM -> String -> String -> Int -> Orc TestResult
    roundTripPing pgr svr pmac smac num =
      pure Results <*> sequence [ pingSent
                                , pingGot
                                , packetSent
                                , packetGot
                                ]
      where
        pingSent, pingGot, packetSent, packetGot :: Orc TestResult

        pingSent   = relay verbose pgr $
          Accept (Just $ "Sent ping #" ++ show num) StdOut pingerTimeout

        pingGot    = relay verbose svr $
          Accept (Just $ unwords [ "Got ping"
                                 , show num
                                 , "from"
                                 , pmac
                                 ])                 StdOut serverTimeout

        packetSent = relay verbose svr $
          Accept (Just "Sent packet:")              StdOut serverTimeout

        packetGot  = relay verbose pgr $
          Accept (Just $ unwords [ "Received response"
                                 , show num
                                 , "from"
                                 , smac
                                 ])                 StdOut pingerTimeout

    serverTimeout, pingerTimeout :: Maybe Int
    serverTimeout = Nothing -- Just $ 10 * seconds
    pingerTimeout = Nothing -- Just $ 10 * seconds

main :: IO ()
main = runTest test
