-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
import Hypervisor.Kernel(halvm_kernel)
import XenDevice.HypervisorConsole(dHypervisorConsole,
                                   writeHypervisorConsole,
                                   getLnHypervisorConsole)
import Data.Char(toUpper)

-- Echo user input mapped to upper case

main = halvm_kernel [dHypervisorConsole] $ \_ ->
  sequence_ $ repeat (getLnHypervisorConsole >>= 
                      (writeHypervisorConsole . map toUpper . (++"\n")))
