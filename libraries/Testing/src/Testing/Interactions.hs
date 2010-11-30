-- BANNERSTART
-- - Copyright 2006-2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND
--
module Testing.Interactions
  ( module Testing.TestResults
  
    -- * Types
  , Interaction(..)
  , OStream(..)

    -- * Functions
  , interaction       -- :: Site -> Interaction -> Orc TestResult

    -- * Constants
    -- ** Standard interactions
  , anyLine           -- :: Interaction
    -- ** Building interactions
  , accept            -- :: String -> Interaction
  , acceptWithin      -- :: String -> Int -> Interaction
  , acceptSeries      -- :: [String] -> Interaction
  , expect            -- :: String -> Interaction
  , expectWithin      -- :: String -> Int -> Interaction
  , expectSeries      -- :: [String] -> Interaction
    
    -- ** Standard interaction components
  , seconds           -- :: Int
  , minutes           -- :: Int
  , indefinitely      -- :: Maybe Int
  , anything          -- :: Maybe String
  , testTimeout       -- :: Int
  , verbalPause       -- :: Maybe Int
    -- ** Reporting results
  , report            -- :: String -> Orc ()
  , recordResult      -- :: TestResult -> Orc ()
  , relay             -- :: Bool -> Site -> Interaction -> Orc TestResult
  )
where

import Orc
import Testing.Site
import Testing.TestResults

import Control.Monad
import Control.Applicative
import Text.Regex.Posix        ( (=~) )
import Text.Regex.Base.Context ()


-- | An "Interaction" describes the expected transcript of the observations
-- by the tester of a conversation between the tester and subject.
data Interaction
  = -- | Feed the given string to the test subject, as input.
    Feed String

    -- | Accept any line of output produced by the given output stream.  If
    -- given a regular expression, keep accepting lines until one matches,
    -- returning that line, paired with all previous lines (in the order they
    -- were read).
  | Accept { acc_until    :: Maybe String -- ^ Regular expression characterizing
                                          -- expected output; currently, ranges
                                          -- over a single line only
           , acc_outs     :: OStream      -- ^ Output stream to listen to
           , acc_mtimeout :: Maybe Int    -- ^ Optional timeout, in milliseconds
           }

    -- | Wait for output from the test subject that matches a given
    -- regular expression.
  | Expect { exp_match    :: String    -- ^ Regular expression characterizing
                                       -- expected output; currently, ranges
                                       -- over a single line only
           , exp_outs     :: OStream   -- ^ Output stream to interrogate
           , exp_mtimeout :: Maybe Int -- ^ Optional timeout, in milliseconds
           }

    -- | A lull in the conversation.
  | Wait Int

    -- | A series of interactions, to be conducted in order.
  | Series [Interaction]

    -- | A set of interaction possibilities, the first of which to succeed
    -- is the result of the interaction (and the other alternatives are
    -- stopped).
  | OneOf [Interaction]

  deriving ( Eq, Ord, Show, Read )


-- | Every sub-process has two kinds of output streams, @stdout@ and @stderr@.
data OStream
  = StdOut
  | StdErr
  deriving ( Eq, Ord, Show, Read )


-- | Read a line from the selected output stream of the given "Site".  Reify
-- any low-level errors as test failures.
readLine :: Site -> OStream -> (String -> Orc TestResult) -> Orc TestResult
readLine site outs success = do
  line <- ioOrc $ byStream outs (site_getLn site) (site_getErrLn site)
  if isSiteError line
     then return $ Failure line
     else success line

-- | Use a given "OStream" to select from two alternatives; the selection is
-- based on the standard ordering derived from "OStream" (that
-- "StdOut" @<@ "StdErr").
byStream :: OStream -> a -> a -> a
byStream StdOut o _ = o
byStream StdErr _ e = e


-- | Render "OStream" in a more Unix-standard format.
printStream :: OStream -> String
printStream StdOut = "<stdout>"
printStream StdErr = "<stderr>"


-- | Interacts with a given "Site", according to the given interaction script.
-- The end result is either a simple success, a success with auxiliary output,
-- or a failure.
interaction :: Site -> Interaction -> Orc TestResult
interaction site script =
  case script of
    Feed s -> do
      res <- ioOrc $ site_putStrLn site s
      case res of
        Just err -> return $ Failure err
        Nothing  -> return $ Success Nothing

    Accept Nothing outs mtimeout -> waitUntil mtimeout timeoutFail $
      readLine site outs $ \ line -> return $ Success $ Just (line, [line])
      where
        timeoutFail :: Int -> [String]
        timeoutFail = renderTimedout "Accept" outs Nothing

    Accept (Just pat) outs mtimeout ->
      waitUntil mtimeout timeoutFail (checkNextLine [])
      where
        checkNextLine :: [String] -> Orc TestResult
        checkNextLine acc =
          readLine site outs $ \ line ->
          -- Operator "(=~)" is the ingeniously overloaded convenience regex
          -- match function from "Text.Regex.Base".  Below, the result type
          -- determines which very different operation is performed.  See
          -- Bryan O'Sullivan's most excellent tutorial for more:
          --   http://tinyurl.com/BOSRegexTute
          if line =~ pat -- :: Bool; "True" if there exists a match
             then return $ Success $ Just (line, reverse acc)
             else checkNextLine (line:acc)

        timeoutFail :: Int -> [String]
        timeoutFail = renderTimedout "Accept" outs (Just pat)

    Expect pat outs mtimeout -> waitUntil mtimeout timeoutFail $
      readLine site outs $ \ line ->
      -- Operator "(=~)" is the ingeniously overloaded convenience regex match
      -- function from "Text.Regex.Base".  Below, the result type determines
      -- which very different operation is performed.  See Bryan O'Sullivan's
      -- most excellent tutorial for more: http://tinyurl.com/BOSRegexTute
      return $
        if line =~ pat             -- :: Bool; "True" if there exists a match
           then Success $
                Just ( line
                     , line =~ pat -- :: [String]; returns all matches
                     )
           else failure line
      where
        failure :: String -> TestResult
        failure line = Failure $ unlines
          [ unwords [ "Expect failed; from"
                    , site_name site
                    , printStream outs
                    , "expected:"
                    ]
          , '\t' : show pat
          , "but saw instead:"
          , '\t' : show line
          ]

        timeoutFail :: Int -> [String]
        timeoutFail = renderTimedout "Expect" outs (Just pat)

    Wait t -> do
      rtimer t -- wait for t ms
      return $ Success Nothing

    Series is ->
      -- perform interactions in order, collecting all results
      pure Results <*> mapM (cut . interaction site) is

    OneOf is ->
      -- perform interactions in parallel, first to finish is the result
      cut $ spawn $ map (cut . interaction site) is

  where
    waitUntil :: Maybe Int         -- timeout limit, if any
              -> (Int -> [String]) -- render "timed out" message
              -> Orc TestResult    -- computation under timeout
              -> Orc TestResult
    waitUntil Nothing  _f m = m    -- wait forever
    waitUntil (Just t)  f m =
      timeout t (timedout $ f t) m -- wait at most t ms
      where
        timedout :: [String] -> TestResult
        timedout = Failure . unlines

    renderTimedout :: String -> OStream -> Maybe String -> Int -> [String]
    renderTimedout what outs Nothing    t =
      [ unwords [ what
                , "failed; timed out after"
                , show t
                , "milliseconds waiting for output from:"
                ]
      , '\t' : unwords [ site_name site
                       , ":"
                       , printStream outs
                       ]
      ]
    renderTimedout what outs (Just pat) t =
      [ unwords [ what
                , "failed; timed out after"
                , show t
                , "milliseconds while waiting to match:"
                ]
      , '\t' : show pat
      , unwords [ "from"
                , site_name site
                , ":"
                , printStream outs
                ]
      ]


-- | The most generic interaction: this will accept any single line of input
-- from @stdout@, and will not time out.
anyLine :: Interaction
anyLine =
  Accept anything StdOut indefinitely

-- | Given a string (which may contain regular expressions), builds
-- the interaction which will only accept that string from @stdout@.
accept :: String -> Interaction
accept s = acceptWithin s Nothing

-- | Given a string (which may contain regular expressions), builds
-- the interaction which will only accept that string from @stdout@,
-- timing out, if specified.
acceptWithin :: String -> Maybe Int -> Interaction
acceptWithin s to = Accept (Just s) StdOut to

-- | Given a list of strings (which may be regular expressions), builds
-- the interaction which will only accept those strings in the given order
-- from @stdout@.
acceptSeries :: [String] -> Interaction
acceptSeries = Series . map accept


-- | Given a string (which may contain regular expressions), builds
-- the interaction which will expect that string on the next line
-- from @stdout@.
expect :: String -> Interaction
expect s = expectWithin s Nothing

-- | Given a string (which may contain regular expressions), builds
-- the interaction which will expect that string on the next line
-- from @stdout@, timing out, if specified.
expectWithin :: String -> Maybe Int -> Interaction
expectWithin s to = Expect s StdOut to

-- | Given a list of strings (which may be regular expressions), builds
-- the interaction which will only accept those strings in the given order
-- from @stdout@.
expectSeries :: [String] -> Interaction
expectSeries = Series . map expect



-- | 'seconds' and 'minutes' simplify interacting with Orc's timeout
-- mechanism, which has a granularity of milliseconds (as opposed to
-- Haskell's, which is based on microseconds.
seconds, minutes :: Int
seconds = 1000 -- Orc's time granularity is in milliseconds
minutes = 60 * seconds

-- | Used to specify no timeout for a given interaction.
indefinitely :: Maybe Int
indefinitely = Nothing

-- | Used to specify no pattern to be matched for "Accept" interactions.
anything :: Maybe String
anything = Nothing

-- | The default test timeout is 10 minutes.
testTimeout :: Int
testTimeout = 10 * minutes

-- | An "um..." in the conversation.
verbalPause :: Maybe Int
verbalPause = Just $ 1 * seconds

-- | Report the given message to @stdout@.  Currently, this handle is not
-- protected at all, so we can get some odd interleavings.
report :: String -> Orc ()
report = ioOrc . putStrLn

-- | Render the result, and report it to @stdout@.  Suffers from the same
-- problem as "report" above.
recordResult :: TestResult -> Orc ()
recordResult = report . ppTestResult

-- | Follows the given script with the given "Site", recording the result if
-- verbose output has been requested.
--
-- Suffers from the same problem as "report" above.
relay :: Bool -> Site -> Interaction -> Orc TestResult
relay verbose site script = do
  res <- interaction site script
  when verbose $ recordResult res
  return res

