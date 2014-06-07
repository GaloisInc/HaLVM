import Common
import Communication.IVC
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary.Put
import Hypervisor.Debug
import Hypervisor.ErrorCodes
import Hypervisor.XenStore

main = handle exceptionHandler $ do
  writeDebugConsole "LEFT: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "LEFT: Starting rendezvous.\n"
  outch <- leftSide xs
  writeDebugConsole "LEFT: Completed rendezvous.\n"
  forM_ [0..65500] $ \next -> do
    when (next `mod` 100 == 0) $ do
      writeDebugConsole $ "LEFT: Sent message " ++ show next ++ "\n"
    put outch (runPut (putWord16be next))
    put outch (runPut (putWord16be next))
    put outch (runPut (putWord16be next))
    put outch (runPut (putWord16be next))
  writeDebugConsole "LEFT: Completed writes. Delaying.\n"
  threadDelay (2 * 1000 * 1000)
  writeDebugConsole "LEFT: Done.\n"

exceptionHandler :: ErrorCode -> IO ()
exceptionHandler ec = do
  writeDebugConsole ("RIGHT: Caught exception: " ++ show ec ++ "\n")

