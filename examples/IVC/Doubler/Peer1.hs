import Common
import Communication.IVC
import Control.Concurrent
import Control.Exception
import Control.Monad
import Hypervisor.Debug
import Hypervisor.ErrorCodes
import Hypervisor.XenStore

main = handle exceptionHandler $ do
  writeDebugConsole "LEFT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "LEFT: Starting rendezvous.\n"
  chan <- leftSide xs
  writeDebugConsole "LEFT: Completed rendezvous.\n"
  forM_ [0..65536] $ \next -> do
    put chan next
    next2 <- get chan
    unless (next2 == (next * 2)) $ fail "LEFT: Improper response!\n"
    when (next `mod` 100 == 0) $ do
      writeDebugConsole (show next ++ "  --->  " ++ show next2 ++ "\n")
  writeDebugConsole "LEFT: Completed operation. Delaying.\n"
  threadDelay (2 * 1000 * 1000)
  writeDebugConsole "LEFT: Done.\n"

exceptionHandler :: ErrorCode -> IO ()
exceptionHandler ec = do
  writeDebugConsole ("RIGHT: Caught exception: " ++ show ec ++ "\n")

