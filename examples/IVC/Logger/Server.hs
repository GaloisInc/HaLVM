import Communication.IVC(OutChannel)
import qualified Communication.IVC as IVC
import Communication.Rendezvous
import Control.Concurrent
import Data.Binary
import Data.Char
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore

newtype Event = Event String

instance Binary Event where
  get = undefined
  put (Event s) = mapM_ put (map word8ify s) >> putWord8 0
   where
    word8ify :: Char -> Word8
    word8ify = fromIntegral . ord

accept :: XenStore -> IO (OutChannel Event)
(_, accept) = peerConnection "logger" 1

main :: IO ()
main = do
  writeDebugConsole "'Server' starting.\n"
  con <- initXenConsole
  xs <- initXenStore
  writeDebugConsole "Building connection.\n"
  outch <- accept xs
  writeDebugConsole "Connection established.\n"
  IVC.put outch (Event "Connection established.")
  threadDelay 500000
  IVC.put outch (Event "First event.")
  threadDelay 500000
  IVC.put outch (Event "Second event.")
  threadDelay 500000
  IVC.put outch (Event "Final event.")
  threadDelay 2500000
