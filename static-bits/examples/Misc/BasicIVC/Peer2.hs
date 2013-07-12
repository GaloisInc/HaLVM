import Common
import Communication.IVC
import Control.Concurrent
import Control.Exception
import Control.Monad
import Hypervisor.Debug
import Hypervisor.ErrorCodes
import Hypervisor.XenStore

main = handle exceptionHandler $ do
  writeDebugConsole "RIGHT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "RIGHT: Starting rendezvous.\n"
  inch <- rightSide xs
  writeDebugConsole "RIGHT: Completed rendezvous.\n"
  forM_ [0..65536] $ \ x -> do
    next <- get inch
    unless (next == x) $ do
      writeDebugConsole $ "ERROR: Read " ++ show next ++ " expected "
                       ++ show x ++ "\n"
      throw EINVAL
  writeDebugConsole "RIGHT: Completed reads. Delaying\n"
  threadDelay (2 * 1000 * 1000)
  writeDebugConsole "RIGHT: Done.\n"

exceptionHandler :: ErrorCode -> IO ()
exceptionHandler ec = do
  writeDebugConsole ("RIGHT: Caught exception: " ++ show ec ++ "\n")

