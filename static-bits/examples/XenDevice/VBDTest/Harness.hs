
module Main where

import Testing.VM

import TestOpts
import System.Environment
import Control.Monad


disk, readOnly, removeable, cdrom, bps, sectors :: String
disk       = "Disk hd[a-z][0-9]:"
readOnly   = "read\\-only: (True|False)"
removeable = "removable: (True|False)"
cdrom      = "cdrom: (True|False)"
bps        = "bytes per sector: [0-9]+"
sectors    = "sectors: [0-9]+"

test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "VBDTest"
      preamble    = [ disk
                    , readOnly
                    , removeable
                    , cdrom
                    , bps
                    , sectors
                    ]
      nugget      = "PASSED"
      numTests    = defaultNumTests

  vbd <- create testSubject (testSubject ++ ".config")
                [ VMExtra $ opt ++ show numTests
                , VMName testSubject
                ]

  diskinfo <- relay verbose vbd $ acceptSeries preamble
  
  synctests <- replicateM numTests $ relay verbose vbd $
               Accept (Just nugget) StdOut Nothing
  
  done <- relay verbose vbd $ Accept (Just "Done!") StdOut Nothing

  return $ summarize $ Results $ diskinfo : done : synctests

main :: IO ()
main = runTest test
