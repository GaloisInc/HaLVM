-- An example demonstrating shutdown / reboot registration
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
  onPoweroff xs $ do putStrLn "I'm shutting down! Bye."
                     poweroff
  onReboot   xs $ do putStrLn "I'm trying to reboot! Bye."
                     reboot
  forever $ threadDelay 100000
