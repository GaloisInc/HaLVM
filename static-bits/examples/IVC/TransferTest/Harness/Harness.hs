
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Jef Bell <jefb@galois.com>
-- BANNEREND

module Main where

import Testing.VM
import Control.Monad
import System.Environment


createVM :: String -> Orc VM
createVM name = 
  create name (name ++ ".config") [ VMExtra "writer=console"
                                  , VMName name
                                  ]

test :: Bool -> Orc TestResult
test verbose = do
  receiver <- createVM "Receiver"
  sender <- createVM "Sender"
 
  rtimer $ 3 * seconds

  spawn [ relay verbose receiver $ acceptSeries receiverScript
        , relay verbose sender   $ acceptSeries senderScript
        ]

senderScript :: [String]
senderScript = 
  [ "SND: Waiting for reference."
  , "SND: Making page."
  , "SND: Sending page."
  , "SND: Page sent."
  , "SND: Done."
  ]

receiverScript :: [String]
receiverScript = -- replicate 6 "RCV:"
  [ "RCV: Making offer."
  , "RCV: Creating reference & granting access."
  , "RCV: Writing reference \\(GrantRef [0-9]+\\)."
  , "RCV: Finishing foreign transfer."
  , "RCV: Completed transfer."
  , "RCV: Page transfer succeeded."
  ]


main :: IO ()
main = runTest test

