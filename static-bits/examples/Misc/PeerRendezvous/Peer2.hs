import Hypervisor.Debug
import Hypervisor.XenStore
import Common

main = do
  writeDebugConsole "RIGHT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "RIGHT: Starting rendezvous.\n"
  _ <- rightSide xs
  writeDebugConsole "RIGHT: Completed rendezvous.\n"
