-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Iavor Diatchki <diatchki@galois.com>
-- BANNEREND
import Hypervisor.Kernel(halvm_kernel)
import XenDevice.HypervisorConsole(dHypervisorConsole,writeHypervisorConsole)
import Hypervisor.Privileged(initIOPorts)

import Device.PCI.Probe
import Device.PCI.Device
import Device.Network.NE2000.Main as NIC
import Device.Network.API.Ethernet(myMAC) 

import Data.List(find)
import Control.Monad(msum)

find_dev :: DevTree -> Maybe (Dev ())
find_dev t = msum (check_bus (devices t) : map (find_dev . snd) (bridges t))
  where check_bus ds  = find found ds
        found d = deviceId i == 0x8029 && vendorId i == 0x10EC 
          where i = devInfo d


main :: IO ()
main = halvm_kernel [dHypervisorConsole] $ \_ -> do
  writeStr "Initializing IO ports: "
  b <- initIOPorts
  writeStrLn (if b then "Success." else "Failure.")

  writeStr "Probing PCI bus ... "
  tree <- probe
  writeStrLn "done."

  drawTree writeStrLn (const Nothing, const Nothing) tree

  maybe (writeStrLn "Could not find network device.") initNIC $ find_dev tree

  where
    initNIC :: Dev () -> IO ()
    initNIC d = do
      writeStr "Initializing card ... "
      mb <- NIC.initPCI writeStrLn d
      maybe (writeStrLn "failed.")
            (\ iface -> writeStrLn ("done; MAC: " ++ show (myMAC iface)))
            mb

writeStr, writeStrLn :: String -> IO ()
writeStr x    = writeHypervisorConsole x 
writeStrLn x  = writeStr (x ++ "\n")

