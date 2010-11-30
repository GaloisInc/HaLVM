-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Magnus Carlsson <magnus@galois.com>
-- BANNEREND
module Testing.Interactions(testProcess,Interaction(..),OStream(..)) where

import Control.Concurrent(forkIO,threadDelay)
import IO(hPutStrLn,hGetContents,hClose,hFlush)
import Control.Concurrent.STM(TVar,newTVarIO,readTVar,writeTVar,
                              atomically,retry,orElse)
import Text.Regex.Posix((=~))
import System.Posix(ProcessID)
import Text.Regex.Base.Context()
import Control.Monad(when)
import IO(Handle,hReady,hGetChar,hPutStr,hWaitForInput,stdout)
import GHC.Conc(unsafeIOToSTM)

testProcess :: Bool -> (Handle,Handle,Handle,ProcessID) -> Interaction -> IO ()
testProcess verbose (i,o,e,p) ia = 
  do co <- newTVarIO ""
     ce <- newTVarIO ""
     ct <- newTVarIO 0
     forkIO (feed "stdout" verbose co o)
     forkIO (feed "stderr" verbose ce e)
     forkIO (stepCT ct)
     let env = Env{ coT = co, ceT = ce, ctimeT = ct }
     interaction i env ia

seconds = 1000000
clockIncrement = 1

stepCT :: TVar Double -> IO ()
stepCT t = sequence_ $ repeat $
  do threadDelay (round (clockIncrement*seconds))
     atomically $ 
       do ct <- readTVar t
          writeTVar t (ct + clockIncrement)

data Interaction =
     Feed String
   | Expect (Maybe Double) OStream String
   | Wait Double
   | Interactions [Interaction]
  deriving (Eq,Ord,Show,Read)  

data OStream = StdOut | StdErr
  deriving (Eq,Ord,Show,Read)

handle StdOut oh eh = oh
handle StdErr oh eh = eh

data Env = Env{ coT :: TVar String
              , ceT :: TVar String
              , ctimeT :: TVar Double
              }

feed :: String -> Bool -> TVar String -> Handle -> IO ()
feed name verbose t h = sequence_ $ repeat $ 
  do hWaitForInput h (-1)
     s <- readChunk 1000 h
     when verbose $ 
       do putStrLn $ "read from "++name++": "++s
     atomically $ do
       ci <- readTVar t
       writeTVar t (ci ++ s)

readChunk :: Int -> Handle -> IO String
readChunk 0 h = return []
readChunk m h =
  do r <- hReady h
     if r then  do c <- hGetChar h
                   r <- readChunk (m-1) h
                   return (c:r)
          else do threadDelay (round (0.1*seconds))
                  r <- hReady h
                  if r then readChunk m h else return []

interaction i env ia =
  case ia of
    Feed s -> do putStrLn $ "Feeding: "++ show s
                 hFlush stdout
                 hPutStr i s
                 hFlush i
    Expect to os r -> 
      do atomically $ writeTVar (ctimeT env) 0
         atomically $ 
             do let cT = handle os (coT env) (ceT env)
                ci <- readTVar cT
                case match ci r of
                  Nothing -> retry
                  Just a -> writeTVar cT a
           `orElse`
             case to of Just t -> 
                          do c <- readTVar (ctimeT env)
                             if c < t then retry
                                      else fail $ "expected: "++show r
                        Nothing -> retry

    Wait t -> 
      atomically $
        do c <- readTVar (ctimeT env)
           when (c < t) retry
    Interactions is -> sequence_ $ map (interaction i env) is

match :: String -> String -> Maybe String
match i r  | i =~ r    = Just a 
           | otherwise = Nothing
  where (b,m,a) = i =~ r :: (String,String,String)
