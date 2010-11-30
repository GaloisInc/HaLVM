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
import Device.PCI.Device(drawTree)

main :: IO ()
main = halvm_kernel [dHypervisorConsole] $ \_ -> 
  do writeStrLn "Initializing IO ports:"
     b <- initIOPorts
     writeStrLn (if b then "Success" else "Failure")
     writeStrLn "Probing PCI bus:"
     tree <- probe
     drawTree writeStrLn (const Nothing, const Nothing) tree

writeStrLn :: String -> IO ()
writeStrLn x  = writeHypervisorConsole (x ++ "\n");

