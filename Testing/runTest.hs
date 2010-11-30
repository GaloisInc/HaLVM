-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Testing.POpen(popen)
import Testing.Interactions(testProcess,Interaction(..),OStream(..))
import System(getArgs)
import System.Posix.Signals(signalProcess,killProcess)
import Control.OldException as CE

main = 
  do l <- getArgs
     let (verbose,diskimg,isfile) = 
           case l of
             [diskimg,isfile] -> (False,diskimg,isfile)
             ["-v",diskimg,isfile] -> (True,diskimg,isfile)
     ps@(_,_,_,pid) <- 
       popen "qemu" ["-nographic","-no-kqemu","-hda",diskimg,"-m","256"] Nothing
     let cleanup = signalProcess killProcess pid
     flip CE.catch (\e -> cleanup >> throwIO e)
       $ do is <- readFile isfile
            testProcess verbose ps (read is)
            cleanup
            putStrLn $ isfile ++ " passed."
