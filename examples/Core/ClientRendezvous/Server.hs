import Control.Concurrent
import Hypervisor.Debug
import Hypervisor.XenStore
import Common

main = do
  writeDebugConsole "SERVER: Initializing XenStore.\n"
  xs  <- initXenStore
  writeDebugConsole "SERVER: Initializing MVar.\n"
  countMV <- newMVar 0
  writeDebugConsole "SERVER: Starting rendezvous.\n"
  runServer xs $ \ _ -> do
    writeDebugConsole "SERVER: Found a client!\n"
    cur <- takeMVar countMV
    putMVar countMV $! cur + 1
  waitFor countMV 3
  writeDebugConsole "SERVER: Got all my clients!\n"

waitFor :: MVar Int -> Int -> IO ()
waitFor mv goal = do
  cur <- takeMVar mv
  if cur == goal
    then return ()
    else do putMVar mv cur
            threadDelay 10000
            waitFor mv goal
