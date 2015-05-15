module Hypervisor.Shutdown where

import Control.Concurrent.MVar
import Control.Monad
import Foreign
import Foreign.C
import Hypervisor.XenStore

newtype ShutdownReason = ShutdownReason { unReason :: CInt } 
  deriving (Eq, Storable)

#include <stdint.h>
#include <xen/sched.h>
#{enum ShutdownReason, ShutdownReason
 , srPoweroff = SHUTDOWN_poweroff
 , srReboot   = SHUTDOWN_reboot
 , srSuspend  = SHUTDOWN_suspend
 , srCrash    = SHUTDOWN_crash
 , srWatchdog = SHUTDOWN_watchdog
 }

onPoweroff :: XenStore -> IO () -> IO ()
onPoweroff xs = shutdownCase xs "poweroff"

onReboot :: XenStore -> IO () -> IO ()
onReboot xs = shutdownCase xs "reboot"

-- | If the shutdown reason matches the @String@ argument, execute the handler.
-- Handler will only fire once. Intended for cleanup and shutdown routines.
shutdownCase :: XenStore -> String -> IO () -> IO ()
shutdownCase xs match go = do
  me   <- xsGetDomId xs
  here <- xsGetDomainPath xs me
  mv   <- newMVar ()
  xsWatch xs (here ++ "/control/shutdown") ("SHUTDOWN_" ++ match) (handler mv)
  where
    handler mv path _ = do
      str <- xsRead xs path
      when (str == match) (takeMVar mv >> go)

poweroff :: IO ()
poweroff = shutdown srPoweroff

reboot :: IO ()
poweroff = shutdown srReboot

foreign import ccall unsafe "entryexit.c shutdown"
  shutdown :: ShutdownReason -> IO ()
