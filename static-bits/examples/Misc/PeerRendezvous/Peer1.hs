import Hypervisor.Debug
import Hypervisor.XenStore
import Common

main = do
  writeDebugConsole "LEFT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "LEFT: Starting rendezvous.\n"
  _ <- leftSide xs
  writeDebugConsole "LEFT: Completed rendezvous.\n"
