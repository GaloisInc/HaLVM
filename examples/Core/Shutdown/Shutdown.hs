-- An example demonstrating shutdown registration
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore
import Hypervisor.Shutdown

main :: IO ()
main = do
  con <- initXenConsole
  xs  <- initXenStore
  onPoweroff xs $ putStrLn "I'm shutting down! Bye."
  onReboot   xs $ putStrLn "I'm trying to reboot! Bye."
  forever $ threadDelay 100000

