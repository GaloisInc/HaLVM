{-# LANGUAGE RankNTypes #-}
-- An example showing how programs can interact with the Xenstore.
-- This is a variant of Xenstore.hs which gives a more compact printout.

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>, Magnus Carlsson <magnus@galois.com>
-- BANNEREND
--
import Control.Monad(unless)
import Hypervisor.Kernel
import XenDevice.Console
import XenDevice.Xenbus as XB

main :: IO ()
main = halvm_kernel [dConsole, XB.dXenbus] main'

main' :: forall a b . a -> IO b
main' _ = loop
  where 
   loop =
    do s <- query "xenbus-ls? "
       xenbus_printkey s `catch` errorLog
       loop

errorLog :: Show a => a -> IO ()
errorLog err = writeConsole $ "Error: " ++ show err ++ "\n"

check :: Monad m => m (XBResult a) -> m a
check m =
  do r <- m
     case r of 
      XB.XBOk a -> return a
      XB.XBError err -> fail err

xenbus_printkey :: String -> IO ()
xenbus_printkey p =
	  do r <- check (XB.xsRead p)
             c <- check (XB.xsDirectory p)
             unless (r == "" && not (null c)) $
                    writeConsole(p++": '"++r++"'\n")
             mapM_ (xenbus_printkey . ((p++"/")++)) c

query :: String -> IO String
query s =
   do writeConsole s
      getLnConsole 

