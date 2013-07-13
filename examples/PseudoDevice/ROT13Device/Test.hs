-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
import XenDevice.Console
import Communication.RingBuffer
import XenDevice.Xenbus
import Hypervisor.Debug
import Hypervisor.Kernel
import Hypervisor.Memory
import Hypervisor.Basics
import ROT13Ring
import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Concurrent
import Data.List

main :: IO ()
main = halvm_kernel_daemon [dConsole,dXenbus] start

start :: [String] -> IO ()
start args = do
  let rounds = case args of
                 [arg] | take 7 arg == "rounds=" -> read $ drop 7 arg
                 _                               -> 5
  
  report $ "# rounds = " ++ show rounds ++ ".\n"
  ring <- client
  report "Connected to server.\n"
  mapM_ (runTest ring) [ 1 .. rounds ]
  report "Done.\n"
  threadDelay 100000 -- time enough for final messages to escape

runTest :: FrontROT13Ring -> Word64 -> IO ()
runTest ring round = 
    do writeDebugConsole $ "Test: reading clear text.\n" 
       (addr, gref, size) <- readUntilDotLine (frbDomId ring)
       writeDebugConsole $ "Test: Sending request #" ++ show round ++ " ...\n"
       resp <- frbRequest ring $ ROT13Request round gref size
       writeDebugConsole "Test: ... got response.\n"
       case resp of
         ROT13Response _ 1 -> do
             writeConsole "START ENCODED BLOCK\n"
             writeBlock addr size
             writeConsole "END ENCODED BLOCK\n"
         ROT13Response _ 0 ->
             writeConsole "FAILURE ENCODING BLOCK\n"
         ROT13Response x y ->
             writeConsole $ "FAILURE: Weird Response " ++ show x ++ "  " ++
                            show y

readUntilDotLine :: DomId -> IO (VPtr a, GrantRef, Word16)
readUntilDotLine dom =
    do page <- allocPage
       ref <- allocRef
       grantAccess ref dom page True
       size <- doReadUntilDot page 0 Nothing Nothing
       return (page, ref, fromIntegral size)

doReadUntilDot :: VPtr a -> Int -> Maybe Char -> Maybe Char -> IO Int
doReadUntilDot _page 4096 _prev _prev2 = return 4096
doReadUntilDot  page off   prev  prev2 =
    do [next] <- readConsole 1
       writeConsole [next]
       case (prev2, prev, next) of
         (Just '\n', Just '.', '\n') ->
             return $ off - 1
         _ ->
             do pokeByteOff page off next
                doReadUntilDot page (off + 1) (Just next) prev

writeBlock :: VPtr a -> Word16 -> IO ()
writeBlock page size = 
    do str <- peekCStringLen (castPtr page, fromIntegral size)
       writeConsole str

report :: String -> IO ()
report s = do
  writeConsole s
  writeDebugConsole $ "Test: " ++ s

