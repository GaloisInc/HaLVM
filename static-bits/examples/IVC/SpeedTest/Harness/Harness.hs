
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


daisyChain :: [a] -> (a, [(a, a)], a)
daisyChain []      =
  error "IVC.SpeedTest.Harness.daisyChain: cannot chain empty list"
daisyChain [daisy] =
  (daisy,        [],                         daisy)
daisyChain daisies =
  (head daisies, zip daisies (tail daisies), last daisies)

channels :: (String, [(String, String)], String) -> [String]
channels (source, links, sink) =
  ("outchan=" ++ source) :
  [ unwords [ "inchan=" ++ linkSource, "outchan=" ++ linkSink]
  | (linkSource, linkSink) <- links
  ] ++
  [ "inchan=" ++ sink ]


chanNameBase :: String
chanNameBase = "SpeedTest"

speedTest :: Bool -> Int -> Orc TestResult
speedTest verbose num_nodes = do
  let (srcs_info, (sink_conf, sink_name, sink_args)) = (init nodes, last nodes)
  -- Create all of the source nodes without a console; we're examining the
  -- output at the sink node.
  mapM_ ( \ ( src_conf, src_name, src_args ) ->
                    create_ src_name (src_conf ++ ".config") src_args
        )
        srcs_info
  sink <- create sink_name (sink_conf ++ ".config") sink_args
  
  results <- replicateM 100 $ relay verbose sink
             (Accept (Just speed) StdOut Nothing) 

  return $ summarize $ Results results

  where
    speed :: String
    speed = "Read [0-9]+ in [0-9]+\\.[0-9]+s   [0-9]+\\.[0-9]+ [ KMG][bB]ps"

    num_chans :: Int
    num_chans = num_nodes - 1

    chan_names :: [String]
    chan_names = [ chanNameBase ++ show num_nodes ++ "_" ++ show chan
                 | chan <- [ 1 .. num_chans ]
                 ]

    chan_args :: [VMArg]
    chan_args = map VMExtra $ channels $ daisyChain chan_names

    vm_names :: [(String, String)]
    vm_names =
      (start, start ++ n)
      :
      zip (repeat middle) (mkMiddleNames [ 1 .. (num_nodes - 2) ])
      ++
      [ (end, end ++ n) ]
      where
        start, middle, end, n :: String
        start  = "Start"
        middle = "Middle"
        end    = "End"
        n      = show num_nodes

        mkMiddleNames :: [Int] -> [String]
        mkMiddleNames xs  =
          case xs of
            []  -> []
            [_] -> [middle ++ n]
            _   -> map ( ((middle ++ n ++ "_") ++) . show ) xs

    nodes :: [(String, String, [VMArg])]
    nodes =
      zipWith ( \ (config, name) chan ->
                (config, name, [ VMName name, chan ])
              )
              vm_names
              chan_args

test :: Bool -> Orc TestResult
test verbose = do
  [run2, run3, run6] <- mapM (speedTest verbose) [ 2, 3, 6 ]

  return $ summarize $ Results [run2, run3, run6]
    -- Hackish way to emit the complete results set


mkSeries :: [String] -> Interaction
mkSeries ss = Series $ map (\s -> Accept (Just s) StdOut Nothing) ss

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
