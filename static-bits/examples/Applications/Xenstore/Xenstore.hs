{-# LANGUAGE RankNTypes #-}
-- An example showing how programs can interact with the Xenstore.
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>
-- BANNEREND
--
import Hypervisor.Kernel
import XenDevice.Console
import XenDevice.Xenbus as XB
import Hypervisor.Debug
import Control.Monad
import Control.Concurrent

main :: IO ()
main = halvm_kernel [dConsole, XB.dXenbus] main'

main' :: [String] -> IO ()
main' _ = writeDebugConsole "Starting!\n" >> loop
  where 
   loop = do
     s <- query "xenbus-ls? "
     if s `elem` [ "q", "quit", "exit", "" ]
        then do
          writeConsole "Done.\n"
          threadDelay 1000
        else do
          xenbus_printdir s 0
          loop

xenbus_printdir :: String -> Int -> IO ()
xenbus_printdir path curDepth =
  do r <- XB.xsDirectory path
     case r of 
      XB.XBOk es -> mapM_ printsubdir es
      XB.XBError err -> writeConsole $ "Error: " ++ err ++ "\n"        
  where printsubdir e =
          do writeConsole (replicate curDepth ' ' ++ e)
             let p' = path ++ "/" ++ e
             printelem p'
             xenbus_printdir p' (curDepth + 1)
        printelem p =
	  do r <- XB.xsRead p
             case r of 
	       XB.XBOk "" -> do writeConsole $  ":\n"
               XB.XBOk v -> writeConsole $  " = " ++ v ++ "\n" 
	       XB.XBError err -> writeConsole $  " Error: " ++ err ++ "\n" 

query :: String -> IO String
query s =
   do writeConsole s
      getLnConsole 
