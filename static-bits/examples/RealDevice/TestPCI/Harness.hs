
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

import Testing.VM
import System.Environment
import Control.Monad


test :: Bool -> Orc TestResult
test verbose = do
  let testSubject = "Main"
      cfg         = "Main.config"
      bootMsg     = "Started domain Main"

  vm <- create testSubject cfg []

  boot <- relay verbose vm $ Accept (Just bootMsg) StdOut (Just $ 10 * seconds)
  prb_init <- relay verbose vm $ acceptSeries probeInit

  -- This next interaction only succeeds because there is at least a single
  -- line between the last PCI node info line and the "done" line, viz:
  --
  --   Using config file "./Main.config".
  --   Started domain Main
  --   Initializing IO ports:
  --   Success
  --   Probing PCI bus:
  --   0:31.3: 8086 283e (rev 3)
  --   0:31.2: 8086 2829 (rev 3)
  --   0:31.1: 8086 2850 (rev 3)
  --   ...
  --   0:0.0: 10de 429 (rev 161)
  --   Probe complete.
  --   Exiting ...
  --
  -- If the penultimate line weren't there, we couldn't write a terminating
  -- test that allowed for any number of PCI nodes to be discovered.  This
  -- is because any expect or accept consumes a line of @stdout@ or @stderr@
  -- from the site's buffer -- even if it doesn't match or times out.  So the
  -- "expect" that fails (see the implementation of "manyFollowedBy" below)
  -- consumes the "Immediate exit ..." line, leaving "done" ready to be
  -- "accept"ed".` 
  --
  -- This is where simple regex matching comes against the need for
  -- backtracking; adding the ability put a line /back/ in a site's buffer
  -- would enable a simple regex-based parser combinator library to be built.
  --
  -- An alternative would be to always match either: "<pciNode> | <done>".
  -- This would require inspection of what matched each time (beyond whether
  -- a match was made), so it's a little more work.  But it would obviate the
  -- need for the spurious exatr line.

  nodes <- manyFollowedBy vm (expectWithin pciNode verbalPause)
                             (accept done)

  return $ summarize $ Results $ [ boot, prb_init ] ++ nodes

  where
    probeInit :: [String]
    probeInit =
      [ "Initializing IO ports:"
      , "Success"
      , "Probing PCI bus:"
      ]

    pciNode :: String
    pciNode =
      "[0-9]+:[0-9]+\\.[0-9]+: [0-9a-f]+ [0-9a-f]+ \\(rev [0-9]+\\)"

    done :: String
    done =
      "Exiting ..."

    manyFollowedBy :: VM -> Interaction -> Interaction -> Orc [TestResult]
    manyFollowedBy vm common term = endBy
      where
        endBy :: Orc [TestResult]
        endBy = do
          one <- many
          if successful one
             then liftM (one:) endBy
             else liftM (:[])  end

        many :: Orc TestResult
        many = relay verbose vm common

        end :: Orc TestResult
        end  = relay verbose vm term 
    
main :: IO ()
main = runTest test
