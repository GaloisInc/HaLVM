import Control.Concurrent
import Control.Monad
import Hypervisor.Debug
import System.CPUTime
import System.Locale

main :: IO ()
main = showTime 32

showTime :: Int -> IO ()
showTime 0 = return ()
showTime x = do
  cputime  <- getCPUTime
  writeDebugConsole $ "[" ++ show x ++ "] Now is: \n"
  writeDebugConsole $ "  " ++ show cputime ++ "\n"
  threadDelay 1000000 -- ~1sec
  showTime (x - 1)
