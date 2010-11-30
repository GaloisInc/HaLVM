
-- BANNERSTART
-- - Copyright 2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND

{- |

  Module      : Testing.Site
  Copyright   : (c) 2009 Galois, Inc.

  Defines a child process -based Orc "Site" state and methods for interacting
  with it, via specialized console-based I/O functions.

-}

module Testing.Site
  ( -- * Types
    -- ** Site State
    Site

    -- *** Visible Site State Elements
  , site_name            -- :: String
  , site_putStr          -- :: String -> IO (Maybe String)
  , site_putStrLn        -- :: String -> IO (Maybe String)
  , site_getLn           -- :: IO String
  , site_getErrLn        -- :: IO String
  , site_verbose         -- :: Bool

{-
    -- *** Hidden Site State Elements
  , site_inh             -- :: Handle
  , site_outh            -- :: Handle
  , site_errh            -- :: Handle
  , site_proch           -- :: ProcessHandle
  , site_outs            -- :: TVar String
  , site_errs            -- :: TVar String
  , site_timer           -- :: TVar Int
-}

    -- * Launch a site
  , createSite           -- :: String -> [String] -> IO Site

    -- * Auxiliary functions
  , isSiteError          -- :: String -> Bool
  )
where


import Testing.Site.Util

import Data.List

import System.IO
import System.IO.Error
import System.Process

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Exception


-- | Encapsulates attributes and methods of a virtual machine.  No presumption
-- made that it's a HaLVM (well, no intentional presumptions).
data Site
  = SiteState { -- Visible state elements

                -- | Name of this "Site" (e.g., according to @xm@, if the
                -- "Site" is a "VM").
                site_name            :: String

                -- | put /to/ this "Site".  The result may include an error
                -- message, if the put was not successful.
              , site_putStr          :: String -> IO (Maybe String)

                -- | put /to/ this "Site", appending a newline.  The result may
                -- include an error message, if the put was not successful.
              , site_putStrLn        :: String -> IO (Maybe String)

                -- | get /from/ this "Site", blocking until a whole line is
                -- read.
              , site_getLn           :: IO String

                -- | get an error /from/ this "Site", blocking until a whole
                -- line is read.
              , site_getErrLn        :: IO String

                -- | Determines level of logging output.
              , site_verbose         :: Bool

                -- Hidden state elements
                --
                -- The following state elements are intended to be hidden;
                -- they're not intended to be exported from the site
                -- instantiaton module.

                -- | The @stdin@ handle created when the "Site" was launched
                -- as a child process; used to put input to the "Site".
              , site_inh             :: Handle

                -- | The @stdout@ handle created when the "Site" was launched
                -- as a child process; used to get output from the "Site".
              , site_outh            :: Handle

                -- | The @stderr@ handle created when the "Site" was launched
                -- as a child process; used to get error output from the
                -- "Site".
              , site_errh            :: Handle

                -- | The @process@ handle created when the "Site" was launched
                -- as a child process
              , site_proch           :: ProcessHandle

                -- | The "TVar" containing all output from the "Site";
                -- populated as it appears by a worker thread.  This is
                -- inspected by "site_getLn".
              , site_outs            :: TVar String

                -- | The "TVar" containing all error output from the "Site";
                -- populated as it appears by a worker thread.  This is
                -- inspected by "site_getErrLn".
              , site_errs            :: TVar String

                -- | Updated by a worker thread, this is to wait for time to
                -- pass within the "Site".
              , site_timer           :: TVar Int

              }

-- | Launch a site, yielding a "Site" state (containing methods for
-- communicating with the site once it's created). The first parameter is
-- the name of the site, and the second is the command line (as a list of
-- words) that will be executed in a child process with "sudo".
createSite :: String -> [String] -> IO Site
createSite name args = do
  (inh, outh, errh, ph) <- runInteractiveProcess sudo args Nothing Nothing


  threadDelay launchDelay -- FIXME: this is a hack to give the site time to
                          -- launch.  There must be a better way.

  mustSucceed =<< getProcessExitCode ph -- If forking the subprocess
                                        -- failed, the site launch fails.  It
                                        -- does so silently at the moment, in
                                        -- the Orc fashion.

  mkSite name inh outh errh ph          -- Put all the elements together
                                        -- in a site state
  where
    launchDelay :: Int
    launchDelay = 1000000 -- 1 second


-- | Populates the "Site" state, given a "Site" name, three file handles
-- (corresponding to @stdin@, @stdout@, and @stderr@ for the child
-- process executing the actual Xen domain.
mkSite :: String
          -- ^ The name of the "Site"

       -> Handle
          -- ^ For puting input to the "Site" console's @stdin@

       -> Handle
          -- ^ For receiving output from the "Site" console's @stdout@
  
       -> Handle 
          -- ^ For receiving output from the "Site" console's @stderr@

       -> ProcessHandle
          -- ^ For inspecting the state of the OS process connected to the
          -- running "Site"

       -> IO Site

mkSite name inh outh errh proch = do
  reader <- newMVar ()

  outs <- newTVarIO ""
  errs <- newTVarIO ""
  timer <- newTVarIO 0 -- ticker may not be needed in the end?

  outT <- forkIO $ listen (name ++ " <stdout>:") outs outh
  errT <- forkIO $ listen (name ++ " <stderr>:") errs errh
  timerT <- forkIO $ ticker timer

  return $ SiteState { site_name            = name
                     , site_putStr          = putTo reader hPutStr   inh
                     , site_putStrLn        = putTo reader hPutStrLn inh
                     , site_getLn           = getFrom "<stdout>" outs
                     , site_getErrLn        = getFrom "<stderr>" errs
                     , site_verbose         = True
                     , site_inh             = inh
                     , site_outh            = outh
                     , site_errh            = errh
                     , site_proch           = proch
                     , site_outs            = outs
                     , site_errs            = errs
                     , site_timer           = timer
                     }
  where
    -- For the moment, we're just writing this to the "Site" console's @stdin@.
    -- Later, we may wish to echo it into the log, etc.
    --
    -- There's potential for a race condition here, if two threads try to
    -- put to a "Site" simultaneously.  This is protected by a synchronization
    -- "MVar", "site_read".
    putTo :: MVar () -> (Handle -> String -> IO ()) -> Handle -> String
          -> IO (Maybe String)
    putTo reader put h s = 
      handle ( \ (e :: IOException) ->
               return $ Just $ name ++ ": " ++ siteError ++ ": " ++ show e ) $
        withMVar reader $ \ _ -> do
          put h s
          hFlush h
          return Nothing

    -- Here, we read from the "Site"'s stdout, up to the next newline (which is
    -- dropped).  Like "putTo", "getFrom" might get echo'd into the log
    -- in a future version.
    --
    -- The choice to read as far as the next newline restricts the range
    -- of regular expressions (which are used to match test results) to a
    -- single line.  This may prove problematic (if we need to match multiple
    -- lines at once).  A simple alternative is:
    --
    -- > getFrom _src buf = atomically $ 
    -- >   do s <- readTVar buf
    -- >      writeTVar buf ""
    -- >      return s
    --
    getFrom :: String -> TVar String -> IO String
    getFrom _src buf = atomically $ do
      s <- readTVar buf
      case break (== '\n') s of
        (_,    "")     ->
          -- we haven't seen a newline yet
          retry
        (line, nl:rest)
          | nl == '\n' -> do
            -- XenDevice.Console translates '\n' into "\n\r" before
            -- relaying output; drop it here
            -- FIXME: perhaps should be changed in XenDevice.Console?
            writeTVar buf $ dropWhile (== '\r') rest
            return line

          | otherwise  ->
            -- according to properties of break, this cannot happen
            error "Testing.Sites.mkSite.getFrom: expected newline not found"


-- | Given a "Tvar", atomically applies a function to the "TVar"'s contents.
modifyTVar :: TVar a -> (a -> a) -> IO ()
modifyTVar t f = atomically $ do
  x <- readTVar t
  writeTVar t $ f x

-- | Number of milliseconds in a second.
microseconds :: Int
microseconds = 1000

-- | Granularity of the "Site"'s clock, in "microseconds".
clockIncrement :: Int
clockIncrement = 1000 -- ms == 1 second

-- | Given a "TVar", increments once every second.
ticker :: TVar Int -> IO ()
ticker t = sequence_ $ repeat $ do
  threadDelay $ clockIncrement * microseconds
  modifyTVar t (clockIncrement +)

-- | Given "TVar" buffer and a handle, fill the buffer with whatever the handle
-- produces.
listen :: String -> TVar String -> Handle -> IO ()
listen name buf h =
  handleJust
    (\ (e :: IOException) ->
       -- We may wish to do more here; an "EOF" here seems to be the best
       -- signal we have that a HaLVM has stopped running.  Basically, the
       -- HaLVM shuts down, which means the "XenDevice.Console" backend
       -- disappears.  The process that forked was to launch the HaLVM via
       -- @xm create@ has three handles attached to the "XenDevice.Console"'s
       -- front-end driver's @stdin@, @stdout@, and @stderr@; these are
       -- actually Unix pipes (so each handle is one end of a pipe).
       --
       -- When the back-end driver shuts down, that leads to the front-end
       -- driver closing its ends of those pipes, leading to "EOF" errors if
       -- we try to use our ends of the pipes.
       if isEOFError e then Just () else Nothing
    )
    (\ _                  -> do
       -- For the moment, just put something informative in the buffer and
       -- stop listening.
       let eofErrMsg = name ++ ": " ++ siteError ++ ": unexpected EOF\n"
       modifyTVar buf (++ eofErrMsg)
    )
  $
  sequence_ $ repeat $ do
  hWaitForInput h (-1)    -- wait indefinitely
  chunk <- readChunk 1000 -- read 1000 chars or until handle no longer
                          -- ready, whichever comes first; 1000 may need
                          -- to be tuned
  -- when verbose $ log $ "read from " ++ name ++ ": " ++ s
  modifyTVar buf (++ chunk)

  where
    -- Given a maxmimum numbers of characters to read, will read up to that
    -- number from the handle.  If the handle is not ready, wait 0.1
    -- seconds; if it's still not ready at that point, then we're done.
    -- Otherwise, try to read another character.
    readChunk :: Int -> IO String
    readChunk 0 = return []
    readChunk n = do
      ready <- hReady h
      if ready then do
             ch <- hGetChar h
             chunk <- readChunk (n-1)
             return (ch:chunk)
           else do
             threadDelay $ 100 * microseconds
             ready_now <- hReady h
             if ready_now then readChunk n else return []

-- | True when the given string looks like a report of a low-level site error.
-- False positives possible, but hopefully unlikely.
isSiteError :: String -> Bool
isSiteError = isInfixOf siteError

siteError :: String
siteError = "site connection error"
  
