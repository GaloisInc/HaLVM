{-# LANGUAGE FlexibleContexts #-}
module Log4h (
    log4hStart
  , log4hShutdown
  , log4hBasicConfig 
  , log4hConfigure
  , Level(..)
  , Logger

  , getLogger
  , getGlobalLogger
  , setLevel
  , removeLevel 
  , setAdditivity
  , addAppender 
  , removeAppender 
  , removeAllAppender 

  , setAppender

  , logLocTrace
  , logLocDebug
  , logLocVerbose
  , logLocInfo
  , logLocWarn
  , logLocError
  , logLocFatal

  , logTrace   -- for developers
  , logDebug   -- for developers
  , logVerbose -- used with verbose option (-v or --verbose)
  , logInfo    -- default level
  , logWarn   
  , logError
  , logFatal

  , whenTrace
  , whenDebug
  , whenVerbose
  , whenInfo
  , whenWarn
  , whenError
  , whenFatal

  , namedForkIO

  , layoutNone
  , layoutSimple
  , layoutTTCC
  , layoutCompiler
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception hiding (catch)
import Control.Monad
import Data.Char
import qualified Data.HashTable as Hash
import Data.Int
import Data.IORef
import Data.List
import Data.Maybe
import Hypervisor.Debug
import System.IO
import System.IO.Unsafe
import System.Locale
import System.Time

data Level
  = ALL
  | TRACE
  | DEBUG
  | VERBOSE
  | INFO
  | WARN
  | ERROR
  | FATAL
  | OFF
  deriving (Enum, Eq, Ord, Read, Show)

type LogName = String
type AppName = String
type Loc = Maybe (String, Int)
type Appender = ClockTime -> LogName -> Loc -> Level -> String -> IO ()
type LogIt = Loc -> Level -> String -> IO ()
type Filter = Level -> Bool

newtype Logger = Logger
  { toIORef :: IORef (LogName, Level, LogIt)
  }

data LogState = LogState
  { lsLogs :: Hash.HashTable LogName LogInfo
  , lsApps :: Hash.HashTable AppName (Appender, Filter)
  , lsStart :: ClockTime
  }

data LogInfo = LogInfo
  { lgLevel :: Maybe Level
  , lgAdditive :: Bool
  , lgApps :: [AppName]
  , lgLogger :: Logger
  , lgChildren :: [LogName]
  }

--------------------------------------------------------------------------------
{-# NOINLINE log4h #-}
log4h :: MVar LogState
log4h = unsafePerformIO newEmptyMVar

{-# NOINLINE log4htid #-}
log4htid :: MVar (Maybe ThreadId)
log4htid = unsafePerformIO (newMVar Nothing)

{-# NOINLINE log4htidtoname #-}
log4htidtoname :: MVar (Hash.HashTable String String)
log4htidtoname = unsafePerformIO (Hash.new (==) namehash >>= newMVar)

-- | Call once to initilize log4h.
log4hStart :: IO ()
log4hStart = do
  logInfo self "start"
  as <- Hash.fromList namehash []
  ts <- Hash.fromList namehash []
  ct <- getClockTime
  putMVar log4h (LogState ts as ct)

-- | Call once to shutdown log4h.
log4hShutdown :: IO ()
log4hShutdown = do
  mbtid <- takeMVar log4htid
  when (isJust mbtid) $ killThread (fromJust mbtid)
  ls <- takeMVar log4h
  as' <- Hash.toList (lsApps ls)
  -- XXX: Should call the close on all appenders.
  logTrace self "deleting appenders"
  mapM_ (\(k, _) -> Hash.delete (lsApps ls) k) as'
  logInfo self "stop"

-- | Log4h's very own logger. Can not log in logLoc and in appenders.
{-# NOINLINE self #-}
self :: Logger
self = unsafePerformIO $ do
  ct <- getClockTime
  logger <- stdoutAppender "%c %-5p [%6r] - %m%n"
  i <- newIORef ("log4h", WARN, logger ct "log4h")
  return (Logger i)

--------------------------------------------------------------------------------

-- | Send output to all named appenders.
compose :: LogState -> LogName -> [AppName] -> IO LogIt
compose ls n as = do
  logTrace self ("compose " ++ show as)
  as' <- mapM (Hash.lookup (lsApps ls)) as
  return (\loc level message -> sequence_
    [ a (lsStart ls) n loc level message | (a, f) <- catMaybes as', f level])

-- | Simple but efficient hash (DJB).
namehash :: String -> Int32
namehash = foldl' (\h c -> h * 33 + fromIntegral (ord c)) 5381

-- | Should be in some standard library.
void :: Monad m => m a -> m ()
void ma = do _ <- ma; return ()

-- | Propagates the inherited attributes down the tree.
fixRoot :: LogState -> IO ()
fixRoot ls = do
  logTrace self "fixRoot start"
  fixTree ls OFF [""] ""
  logTrace self "fixRoot stop"

fixTree :: LogState -> Level -> [AppName] -> LogName -> IO ()
fixTree ls l a n = do
  logDebug self ("fixTree at node " ++ show n)
  mbli <- Hash.lookup (lsLogs ls) n
  case mbli of
    Nothing -> return ()
    Just li -> do
      Hash.lookup (lsLogs ls) n
      let
        l' = maybe l id (lgLevel li)
        a' = if lgAdditive li then a ++ lgApps li else lgApps li
      fs <- compose ls n a'
      writeIORef (toIORef (lgLogger li)) (n, l', fs)
      mapM_ (fixTree ls l' a') (lgChildren li)

-- | Create a logger and its parents.
addNode :: LogState -> LogName -> IO Logger
addNode ls n = do
  let parent = (reverse . drop 1 . dropWhile (/='.') . reverse) n
  unless (null n) $ do
    mbp <- Hash.lookup (lsLogs ls) parent
    when (isNothing mbp) $ void $ addNode ls parent
    Just pli <- Hash.lookup (lsLogs ls) parent
    logDebug self ("addNode adding child to " ++ show parent)
    void $ Hash.update (lsLogs ls) parent pli{ lgChildren = n: lgChildren pli }

  logDebug self ("addNode adding " ++ show n)
  i <- newIORef (uninit,uninit,uninit)
  Hash.insert (lsLogs ls) n (LogInfo Nothing True [] (Logger i) [])
  return (Logger i)
  where
  uninit = error "addNode:uninitialized IORef"

{-
-- | Show the log state.
showLogState :: IO ()
showLogState = do
  ls <- takeMVar log4h
  as' <- Hash.toList (lsApps ls)
  logDebug self ("showLogState appenders: " ++ show (map fst as'))
  ts' <- Hash.toList (lsLogs ls)
  ts'' <- mapM toString ts'
  mapM_ (logDebug self . ("showLogState"++)) $ sort ts''
  putMVar log4h ls

toString :: (LogName, LogInfo) -> IO String
toString (n, li) = do 
--  (_, rl, _) <- readIORef (toIORef lg)
  return (show n ++ " " ++ (maybe "+" show (lgLevel li)) ++ " " ++
    (if lgAdditive li then "+" else "") ++ show (lgApps li))
     -- ++ " >=" ++ show rl)
-}
--------------------------------------------------------------------------------

-- | Create or get an existing logger.
getLogger :: LogName -> IO Logger
getLogger n = do
  ls <- takeMVar log4h
  logDebug self ("getLogger getting logger " ++ show n)
  mbli <- Hash.lookup (lsLogs ls) n
  lg <- case mbli of
    Nothing -> do
      logDebug self ("getLogger new logger " ++ show n)
      lg <- addNode ls n
      fixRoot ls
      return (lg)
    Just li -> do
      logDebug self ("getLogger got logger " ++ show n)
      return (lgLogger li)
  putMVar log4h ls
  return (lg)

-- | Uses unsafePerformIO to create a global variable.
{-# NOINLINE getGlobalLogger #-}
getGlobalLogger :: LogName -> Logger
getGlobalLogger n = unsafePerformIO $ getLogger n

-- | Update an appender.
setAppender :: AppName -> IO Appender -> IO ()
setAppender n ma = do
  a <- ma
  ls <- takeMVar log4h
  logDebug self ("setAppender " ++ show n)
  mba <- Hash.lookup (lsApps ls) n
  case mba of
    Nothing -> Hash.insert (lsApps ls) n (a, \_ -> True)
    Just (_, f) -> void $ Hash.update (lsApps ls) n (a, f)
  fixRoot ls
  putMVar log4h ls

-- | Update a filter.
setFilter :: AppName -> (Level -> Bool) -> IO ()
setFilter n f = do
  ls <- takeMVar log4h
  logDebug self ("setFilter " ++ show n)
  mba <- Hash.lookup (lsApps ls) n
  case mba of
    Nothing -> Hash.insert (lsApps ls) n (\_ _ _ _ _ -> return (), f)
    Just (a, _) -> void $ Hash.update (lsApps ls) n (a, f)
  fixRoot ls
  putMVar log4h ls

-- | Update a logger.
updateLogger :: Logger -> (LogInfo -> LogInfo) -> IO ()
updateLogger lg f = do
  (n,_,_) <- readIORef (toIORef lg)
  ls <- takeMVar log4h
  logDebug self ("updateLogger updating information for " ++ show n)
  mbli <- Hash.lookup (lsLogs ls) n
  when (isJust mbli) $ void $ Hash.update (lsLogs ls) n (f (fromJust mbli))
  fixRoot ls
  putMVar log4h ls

-- | Change the level of a logger.
setLevel :: Logger -> Level -> IO ()
setLevel lg l = updateLogger lg (\li -> li{lgLevel = Just l})

-- | Make the logger inherit the level.
removeLevel :: Logger -> IO ()
removeLevel lg = updateLogger lg (\li -> li{lgLevel = Nothing})

-- | Change the additivity of a logger.
setAdditivity :: Logger -> Bool -> IO ()
setAdditivity lg add = updateLogger lg (\li -> li{lgAdditive = add})

-- | Add an appender to a logger.
addAppender :: Logger -> AppName -> IO ()
addAppender lg app = updateLogger lg (\li -> li{lgApps=lgApps li `union` [app]})

-- | Remove an appender from a logger.
removeAppender :: Logger -> AppName -> IO ()
removeAppender lg app = updateLogger lg (\li -> li{lgApps = lgApps li \\ [app]})

-- | Remove all appender from a logger.
removeAllAppender :: Logger -> IO ()
removeAllAppender lg = updateLogger lg (\li -> li{lgApps = []})

-- | logLoc is the primitive logger.
logLoc :: Logger -> Loc -> Level -> String -> IO ()
logLoc lg mbloc priority msg = do
  (_, level, appender) <- readIORef (toIORef lg)
  when (priority >= level) $
    appender mbloc priority msg

-- | The logging functions.
logLocTrace, logLocDebug, logLocVerbose, logLocInfo, logLocWarn, logLocError, logLocFatal
  :: Logger -> (String, Int) -> String -> IO ()
logLocTrace l loc m   = logLoc l (Just loc) TRACE m
logLocDebug l loc m   = logLoc l (Just loc) DEBUG m
logLocVerbose l loc m = logLoc l (Just loc) VERBOSE m
logLocInfo l loc m    = logLoc l (Just loc) INFO m
logLocWarn l loc m    = logLoc l (Just loc) WARN m
logLocError l loc m   = logLoc l (Just loc) ERROR m
logLocFatal l loc m   = logLoc l (Just loc) FATAL m

-- | The simplified logging functions without location information.
-- logIt :: Logger -> Level -> String -> IO ()
-- logIt lg = logLoc lg Nothing

logTrace, logDebug, logVerbose, logInfo, logWarn, logError, logFatal
  :: Logger -> String -> IO ()
logTrace l m   = logLoc l Nothing TRACE m
logDebug l m   = logLoc l Nothing DEBUG m
logVerbose l m = logLoc l Nothing VERBOSE m
logInfo l m    = logLoc l Nothing INFO m
logWarn l m    = logLoc l Nothing WARN m
logError l m   = logLoc l Nothing ERROR m
logFatal l m   = logLoc l Nothing FATAL m

-- | Check if a level is enabled.
whenLevel :: Logger -> Level -> IO () -> IO ()
whenLevel lg priority ma = do
  (_, level, _) <- readIORef (toIORef lg)
  when (priority >= level) ma

whenTrace, whenDebug, whenInfo, whenVerbose, whenWarn, whenError, whenFatal
  :: Logger -> IO () -> IO ()
whenTrace l   = whenLevel l TRACE
whenDebug l   = whenLevel l DEBUG
whenVerbose l = whenLevel l VERBOSE
whenInfo l    = whenLevel l INFO
whenWarn l    = whenLevel l WARN
whenError l   = whenLevel l ERROR
whenFatal l   = whenLevel l FATAL

-- | Use this to print a name instead of ThreadId.
namedForkIO :: String -> IO () -> IO ThreadId
namedForkIO name ma = forkIO nma
  where
    nma = do
      tid <- myThreadId
      let ntid = show tid
      bracket_
        (readMVar log4htidtoname >>= \ht -> Hash.insert ht ntid name)
        (readMVar log4htidtoname >>= \ht -> Hash.delete ht ntid)
        ma

-- | consoleAppender logs to a file descriptor using the supplied layout format.
consoleAppender :: String -> IO Appender
consoleAppender format = return $ \ct name loc level message -> do
  output <- layout format ct name loc level message
  void $ writeDebugConsole output

stdoutAppender :: String -> IO Appender
stdoutAppender = consoleAppender

stderrAppender :: String -> IO Appender
stderrAppender = consoleAppender

fileAppender :: String -> String -> IO Appender
fileAppender _ format = do
  consoleAppender format

-- | layout formats the log message according to a format string.
layout :: String -> ClockTime -> LogName -> Loc -> Level -> String -> IO String
layout fmt ct name loc level message = liftM concat (format fmt)
  where
    format "" = return ([])
    format ('%':xs) = do
      let (p, (t, xs')) = modifier xs
      if (null xs')
       then return ([])
       else do
         r <- convert (head xs')
         rs <- format (tail xs')
         return (pad p (chop t r):rs)
    format (x:xs) = do rs <- format xs; return ([x]:rs)

    convert 'c' = return (name)
    convert 'd' = do
      t <- getClockTime
      c <- toCalendarTime t
      return (formatCalendarTime defaultTimeLocale
        (iso8601DateFormat (Just "%H:%M:%S")) c)
    convert 'F' = case loc of
      Nothing -> return ("")
      Just (f, _) -> return (f)
    convert 'L' = case loc of
      Nothing -> return ("")
      Just (_, l) -> return (show l)
    convert 'l' = case loc of
      Nothing -> return ("")
      Just (f, l) -> return (f ++ ":" ++ show l ++ ": ")
    convert 'm' = return (message)
    convert 'n' = return ("\n")
    convert 'p' = return (show level)
    convert 'P' = return (map toLower (show level))
    convert 'r' = do
      c <- getClockTime
      let (TOD cs cps, TOD ss sps) = (c, ct)
      return (show (1000*(cs-ss) + (cps-sps) `div` 1000000000))
    convert 't' = do
      tid <- myThreadId
      let ntid = show tid
          stid = drop 9 ntid
      ht <- readMVar log4htidtoname
      mbname <- Hash.lookup ht ntid
      return (maybe stid (\n -> n ++ "." ++ stid) mbname)
    convert '%' = return ("%")
    convert c = return ("***" ++ [c] ++ "***")

    modifier ('-':cs) = (Just (-(read n)), modifiertrunc rs)
      where (n, rs) = span isDigit cs
    modifier (c:cs)
      | isDigit c = (Just (read n), modifiertrunc rs)
      where (n, rs) = span isDigit (c:cs)
    modifier cs = (Nothing, modifiertrunc cs)

    modifiertrunc ('.':cs) = (Just (read n), rs)
      where (n, rs) = span isDigit cs
    modifiertrunc cs = (Nothing, cs)

    pad Nothing s = s
    pad (Just n) s
      | n > 0 && n > ls = replicate (n - ls) ' ' ++ s
      | n < 0 && (-n) > ls = s ++ replicate (-n - ls) ' '
      | otherwise = s
      where ls = length s

    chop Nothing s = s
    chop (Just n) s = reverse (take n (reverse s))

-- | Standard layouts.
layoutNone, layoutSimple, layoutTTCC, layoutCompiler :: String
layoutNone = "%m%n"
layoutSimple = "%p - %m%n"
layoutTTCC = "%r [%t] %-5p %c - %m%n"
layoutCompiler = "%l%P: %m%n"

--------------------------------------------------------------------------------
-- Configuration file

-- | The basic configuration sends INFO and lower level to stout and
-- warn and above to stderr.
log4hBasicConfig :: IO ()
log4hBasicConfig = log4hConfigure
    [ "=stdout:stdout:-INFO:%m%n"
    , "=stderr:stderr:WARN-:%m%n"
    , "@:INFO::stdout stderr"
    ]

-- The first character of the line determines the action.
-- '#' to the end of the line is a comment line.
--   E.g. "# This is a comment."
-- '=' followed by name, file, range, and layout fields makes an Appender.
--   E.g. "=cli:stdout:DEBUG-INFO:%r - %m%n"
-- '@' followed by name, level, additivity, and appender fields makes a Logger.
--   E.g. "@com.example.log4h.lg:INFO:!:out1 out2 appx appy"

log4hConfigure :: [String] -> IO ()
log4hConfigure s = mapM_ config s
  where
  config ('#':_)   = return ()
  config ('@':cs0) = case fields cs0 of
    [name, level, additivity, apps] -> 
      do
      lg <- getLogger name
      unless (null level) $ setLevel lg (priority level)
      unless (null additivity) $ setAdditivity lg (additivity /= "!")
      mapM_ (addAppender lg) (words apps)
    _ -> 
      logWarn self ("parse failed: " ++ show cs0)
  config ('=':cs0) = case fields cs0 of
    (name: file: range: fmts) -> 
      do
      let fmt = concat (intersperse ":" fmts)
      case file of
        "" -> setAppender name (stdoutAppender fmt)
        "stdout" -> setAppender name (stdoutAppender fmt)
        "stderr" -> setAppender name (stderrAppender fmt)
        f -> setAppender name (fileAppender f fmt)
      unless (null range) $ setFilter name (mkFilter range)
    _ -> 
      logWarn self ("parse failed: " ++ show cs0)
  config cs0 = logWarn self ("parse failed: " ++ show cs0)

  fields cs = case break (==':') cs of
    (f, ':':rs) -> f: fields rs
    (f, _) -> [f]

  priority cs = case (map toLower cs) of
    "all"     -> ALL
    "trace"   -> TRACE
    "debug"   -> DEBUG
    "verbose" -> VERBOSE
    "info"    -> INFO
    "warn"    -> WARN
    "error"   -> ERROR
    "fatal"   -> FATAL
    "off"     -> OFF
    _         -> ALL

  mkFilter cs = case break (=='-') cs of
    ("", '-':hi) -> \level -> level <= priority hi
    (lo, "-") -> \level -> level >= priority lo
    (lo, '-':hi) -> \level -> level <= priority hi && level >= priority lo
    (lo, "") -> \level -> case priority lo of
      OFF -> False
      ALL -> True
      pri -> level == pri
    _ -> \_ -> True
