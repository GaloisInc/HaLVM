
module Main where

import Testing.VM
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
  [ "SND: Waiting for accept."
  , "SND: Waiting for reference."
  , "SND: Making page."
  , "SND: Copying page."
  , "SND: Page copied."
  , "SND: Done."
  ]

receiverScript :: [String]
receiverScript = 
  [ "RCV: Making offer."
  , "RCV: Creating destination page & granting access."
  , "RCV: Writing reference."
  , "RCV: Waiting for completed transfer."
  , "RCV: Page successfully copied after [0-9]+ tries."
  ]

main :: IO ()
main = runTest test

