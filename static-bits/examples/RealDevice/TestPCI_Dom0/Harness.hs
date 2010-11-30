
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

module Main where

import Testing.QEMU
import System.Environment
import Control.Monad


test :: Bool -> Orc TestResult
test verbose = do
  let testSubject  = "Main"
      disk         = "Main.img"
      bootMsg      = "Booting HaLVM ... done."
  qm <- create testSubject $ stdQEMUArgs { qemu_disk   = disk
                                         , qemu_serial = Just "stdio"
                                         }

  boot <- relay verbose qm $ Accept (Just bootMsg) StdOut (Just $ 10 * seconds)
  prb_init <- relay verbose qm $ acceptSeries probeInit

  -- This next interaction only succeeds because there is at least a single
  -- line between the last PCI node info line and the "halvmExit" line, viz:
  --
  --   Booting HaLVM ... done.
  --   MM: Increased memory reservation to 3 of 4096 MB.
  --   Initializing IO ports:
  --   Success
  --   Probing PCI bus:
  --   0:3.0: 10ec 8139 (rev 32)
  --   0:2.0: 1013 b8 (rev 0)
  --   0:1.3: 8086 7113 (rev 0)
  --   0:1.1: 8086 7010 (rev 0)
  --   0:1.0: 8086 7000 (rev 0)
  --   0:0.0: 8086 1237 (rev 2)
  --   Immediate exit by halvm_shutdown.
  --   Do_exit called!
  --
  -- If the penultimate line weren't there, we couldn't write a terminating
  -- test that allowed for any number of PCI nodes to be discovered.  This
  -- is because any expect or accept consumes a line of @stdout@ or @stderr@
  -- from the site's buffer -- even if it doesn't match or times out.  So the
  -- "expect" that fails (see the implementation of "manyFollowedBy" below)
  -- consumes the "Immediate exit ..." line, leaving "halvmExit" ready to be
  -- "accept"ed".` 
  --
  -- This is where simple regex matching comes against the need for
  -- backtracking; adding the ability put a line /back/ in a site's buffer
  -- would enable a simple regex-based parser combinator library to be built.
  nodes <- manyFollowedBy qm (expectWithin pciNode verbalPause)
                             (accept halvmExit)
  destroy qm

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

    halvmExit :: String
    halvmExit =
      "Do_exit called\\!"

    manyFollowedBy :: QEMU -> Interaction -> Interaction -> Orc [TestResult]
    manyFollowedBy qm common term = endBy
      where
        endBy :: Orc [TestResult]
        endBy = do
          one <- many
          if successful one
             then liftM (one:) endBy
             else liftM (:[])  end

        many :: Orc TestResult
        many = relay verbose qm common

        end :: Orc TestResult
        end  = relay verbose qm term 
    
main :: IO ()
main = runTest test

