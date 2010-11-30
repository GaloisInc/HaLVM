-- A demo program showing how domains can be interposed between other
-- domains to perform useful tasks. In this example, a filtering 
-- domain is placed between two domains, to restrict certain bits of
-- information from flowing from one to the other.
--
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andrew Tolmach <apt@galois.com>
-- BANNEREND
--
import Hypervisor.Kernel
import XenDevice.Console
import qualified XenDevice.Xenbus as XB
import Communication.IVC(get, put, OutChannel,InChannel)
import System.Time
import qualified System.Exit
import Data.Char(isSpace)
import RendezvousLib.PeerToPeer(p2pConnection)

import Dirty

-----------------------------------------------------------------

{- Specification of 3-player "dirty words" demo -}

type Reader = IO Char
type Writer = Char -> IO ()

{- Read characters from console and write to B -}
playerA :: Writer -> IO ()
playerA writeToB = 
  repeatM_ $
     do c <- getCharConsole
        writeToB c

{- Read words from A, downgrade with logging to console, and optionally write to C -}
playerB :: (Reader,Writer) -> IO ()
playerB (readFromA,writeToC) =
  repeatM_ $
    do (word,term) <- getWord readFromA
       if isDirty word
         then
           do logToConsole $ "removed word: " ++ word 
	      mapM_ writeToC $ "X" ++ [term]
         else 
           mapM_ writeToC $ word ++ [term]
    

{- Read characters from B and echo to console. -}
playerC :: Reader -> IO ()
playerC readFromB = 
  repeatM_ $ 
    do c <- readFromB
       writeConsole [c]

-----------------------------------------------------------------------------------------------

main :: IO ()
main = halvm_kernel_daemon [dConsole, XB.dXenbus] main'

main' :: [String] -> IO ()
main' args = 
  case args of
    ["player=A"] -> setupA >>= playerA
    ["player=B"] -> setupB >>= playerB
    ["player=C"] -> setupC >>= playerC
    _ -> do writeConsole $ "Unrecognized role: " ++ (show args) ++ "\n"
            System.Exit.exitFailure

makeABOutChannel :: IO (OutChannel Char)
makeABInChannel  :: IO (InChannel  Char)
makeBCOutChannel :: IO (OutChannel Char)
makeBCInChannel  :: IO (InChannel  Char)
(makeABOutChannel,makeABInChannel) = p2pConnection "FilterA-BComm"
(makeBCOutChannel,makeBCInChannel) = p2pConnection "FilterB-CComm"

setupA :: IO Writer
setupA = do writeConsole "I'm player A\n"
            c <- makeABOutChannel
            writeConsole "Setup concluded.\n"
            return (put c)

setupB :: IO (Reader,Writer)
setupB = 
    do writeConsole "I'm player B\n"
       abR <- makeABInChannel
       writeConsole "Built read part.\n"
       bcW <- makeBCOutChannel
       writeConsole "Built write part. Setup concluded.\n"
       return (get abR, put bcW)

setupC :: IO Reader
setupC =
    do writeConsole "I'm player C\n"
       c <- makeBCInChannel
       writeConsole "Setup concluded.\n"
       return (get c)

----------------------------------------------------

{- Repeat a monadic action indefinitely -}
repeatM_ :: Monad m => m a -> m ()
repeatM_ m = sequence_ $ repeat m

{- Log a string to console, tagging with current time. -}

logToConsole :: String -> IO ()
logToConsole s = 
  do ct <- getUTCTime
     writeConsole $ ct ++ " " ++ s ++ "\n"

{- Get the current UTC time-of-day -}
getUTCTime :: IO String
getUTCTime =
  do ct <- getClockTime
     return $ calendarTimeToString (toUTCTime ct)


{- Read and echo a single character from the console. -}
getCharConsole :: IO Char
getCharConsole =
  do [c] <- readConsole 1
     writeConsole [c]
     return c

{- Read a word and its terminating character -}
getWord ::  Reader -> IO (String,Char)
getWord reader =
  do c <- reader
     if (isSpace c)
       then
         return ([],c)
       else
         do (cs,t) <- getWord reader
            return (c:cs,t)
{-
Alternative for Player A:

playerA writeToB = 
    sequence_ $ repeat (getLnConsole >>= writeToB >> writeToB "\n")

getLnConsole  =
  do [c] <- readConsole 1
     writeConsole [c]
     if c == '\n' 
       then 
         return [] 
       else 
         do cs <- getLnConsole
            return $ c:cs
-}


{-
Alternative for playerB:

censor' readAll writer tracer = 
   do s <- readAll
      let ws = words s
      let ws' = filter (\w -> not (w `elem` dirty))  ws
      let ws'' =  map (++ " ") ws'
      mapM_ writer ws''
   where dirty = ["dirty","bad","evil"]
-}


