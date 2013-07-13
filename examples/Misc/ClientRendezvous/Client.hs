import Hypervisor.Debug
import Hypervisor.XenStore
import Common

main = do
  writeDebugConsole "CLIENT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "CLIENT: Starting rendezvous.\n"
  _ <- runClient xs
  writeDebugConsole "CLIENT: Completed rendezvous.\n"
