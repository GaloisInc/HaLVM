-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
--
import Control.Monad
import Communication.IVC
import Hypervisor.Kernel
import RendezvousLib.PeerToPeer
import XenDevice.Console
import XenDevice.Xenbus

accept :: IO (InOutChannel String String)
(_, accept) = p2pConnection "c_rot13"

main :: IO ()
main = halvm_kernel [dXenbus,dConsole] $ \ _ -> do
  c <- accept
  writeConsole "Connected.\n"
  forever $ do
    writeConsole "Next line: "
    ln <- getLnConsole
    put c ln
    en <- get c
    writeConsole $ "ENC: " ++ (show en) ++ "\n"
