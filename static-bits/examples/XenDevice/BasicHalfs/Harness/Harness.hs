
module Main where

import Testing.VM

createVM :: String -> Orc VM
createVM name = 
  create name (name ++ ".config") [ VMExtra "writer=console"
                                  , VMName name
                                  ]

test :: Bool -> Orc TestResult
test verbose = do
  receiver <- createVM "Receiver"
  sender <- createVM "Sender"
 
  senderRes <- interaction sender $ acceptSeries senderScript
  receiverRes <- interaction receiver $ acceptSeries receiverScript
  return $ Results [senderRes, receiverRes] 
     -- Hackish way to emit the complete results set
 
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
