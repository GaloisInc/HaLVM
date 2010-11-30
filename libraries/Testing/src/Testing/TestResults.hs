-- BANNERSTART
-- - Copyright 2006-2009, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Andy Adams-Moran <adams-moran@galois.com>
-- BANNEREND
--
module Testing.TestResults
  ( -- * Types
    TestResult(..)

    -- * Functions
    -- ** Rendering results
  , ppTestResult      -- :: TestResult -> IO ()

    -- ** Handling results
  , succeed           -- :: MonadPlus m => m TestResult -> m ()
  , succeedBy         -- :: MonadPlus m => (TestResult -> Bool)
                      --                   -> m TestResult -> m ()
  , successful        -- :: TestResult -> Bool
  , summarize         -- :: TestResult -> TestResult
  , matched           -- :: String -> TestResult -> Bool
  , runTest           -- :: (Bool -> Orc TestResult) -> IO ()

  )
where


--import Control.Monad
import System.Environment
import Orc


-- | A test will result in a "Success" (which sometimes has associated output),
-- or a "Failure" (which includes what was expected).
data TestResult
  = -- | The test succeeded as expected; if the test subject produced output,
    -- it is included in the test result, along with all matches (to the
    -- expected output regular expresssion; see "Expect" in the "Interaction"
    -- type).
    Success (Maybe (String, [String]))

    -- | The test did not run as expected; includes information about the
    -- failure.
  | Failure String

    -- | A set of test results.
  | Results [TestResult]

  deriving ( Eq, Ord, Show, Read )


-- | Pretty print a test result.
ppTestResult :: TestResult -> String
ppTestResult (Results ts)            = unlines $ map ppTestResult ts
ppTestResult (Success Nothing)       = "Success"
ppTestResult (Success (Just (s,ss))) = unlines $
  [    "Success with output: " ++ s
  ,    "            matches: "
  ]
  ++
  map ("                     " ++) ss
ppTestResult (Failure s)             = "Failure: " ++ s



-- | Given a computation that yields a test result, will fail if any part of
-- the test is not successful.
succeed :: MonadPlus m => m TestResult -> m ()
succeed =
  succeedBy successful

-- | Given a predicate over test results, and a computation yielding a test
-- result, succeeds when the predicate over the test result succeeds.
succeedBy :: MonadPlus m => (TestResult -> Bool) -> m TestResult -> m ()
succeedBy p m =
  guard =<< return p `ap` m

-- | Returns "True" iff all tests pass.
successful :: TestResult -> Bool
successful r =
  case r of
    Success{}  -> True
    Failure{}  -> False
    Results rs -> all successful rs

-- | If a test has been successful, summarize that as a simple 'Success'.
-- Otherwise, the test can't be usefully summarized (i.e., "all failed" is not
-- useful).
summarize :: TestResult -> TestResult
summarize r =
  if successful r then Success Nothing else r

-- | Returns "True" if the given pattern is one of the matches returned by the
-- given test results.
matched :: String -> TestResult -> Bool
matched pat r =
  case r of
    Success (Just (_, matches)) -> pat `elem` matches
    _                           -> False


runTest :: (Bool -> Orc TestResult) -> IO ()
runTest test = do
  argv <- getArgs
  let verbose = "--verbose" `elem` argv || "-v" `elem` argv 
  runOrc (putStrLn . ppTestResult) (test verbose)

