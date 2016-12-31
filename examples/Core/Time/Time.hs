import Control.Concurrent
import Control.Monad
import Data.Time
import Hypervisor.Console
import System.CPUTime
import System.Environment
import Data.Time.Format

main :: IO ()
main = do
  con <- initXenConsole
  args <- getArgs
  let count = case args of
               [arg] | take 6 arg == "count=" -> read $ drop 6 arg
               _                              -> 100
  replicateM_ count $ do
    utc_time <- getZonedTime
    cputime  <- getCPUTime
    writeConsole con $ formatTime defaultTimeLocale "%c%n" utc_time
    writeConsole con $ show cputime ++ "\n"
    threadDelay (1 * 1000 * 1000)
