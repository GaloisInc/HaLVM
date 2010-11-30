import Control.Concurrent
import Control.Monad
import Data.Time
import Hypervisor.Kernel
import System.CPUTime
import System.Locale
import XenDevice.Console

main :: IO ()
main = halvm_kernel [dConsole] start

start :: [String] -> IO ()
start args = do
  let count = case args of
               [arg] | take 6 arg == "count=" -> read $ drop 6 arg
               _                              -> 100
  replicateM_ count $ do
    utc_time <- getZonedTime
    cputime  <- getCPUTime
    writeConsole $ formatTime defaultTimeLocale "%c%n" utc_time
    writeConsole $ show cputime ++ "\n"
    threadDelay (1 * 1000 * 1000)
