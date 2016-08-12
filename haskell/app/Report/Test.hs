{-# LANGUAGE TypeFamilies, ViewPatterns #-}

module Report.Test
  ( TestReport(Test)
  ) where

import Args.PbIds
import Command
import Config
import Present
import Problem
import Problems
import Report.Command

import Batteries
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Maybe

-- | Problem data for testing.
data PbTest = PbTest
  { pbId    :: PbId
  , report  :: PbReport
  , answer  :: Result
  }

-- | Make a `PbTest` from a `Problem`.
toPbTest :: Problem -> PbTest
toPbTest (Pb i _ rep mAns) = PbTest i rep ans
  where ans = fromMaybe throwErr mAns
        throwErr = error "toPbTest: problem has no answer"

-- | Test report.
data TestReport = Test

-- | Configuration for test reports.
type TestConfig = Config TestReport

instance Command TestReport where
  cmdName = const "test"

instance Configurable TestReport where
  data Config TestReport = TestConfig
    { pbIds  :: PbIds }
  defaultConfig = TestConfig
    { pbIds = DefaultIds }
  parseOption = const2 Nothing
  parseArg cfg arg = updateCfg . ListIds <$> parsePbIdS arg
    where updateCfg newPbIds = cfg { pbIds = pbIds cfg <> newPbIds }

instance ReportCmd TestReport where
  report cfg@(pbIds -> ListIds l) = testReport cfg { pbIds = ListIds (dedup l) }
  report cfg                      = testReport cfg

-- | Generate an test report with a given configuration.
-- The problems specified in the configuration are run and
-- a report of their correctness is printed.
testReport :: TestConfig -> IO ()
testReport cfg = do
  warnPbNotFound badIds
  warnPbHasNoAnswer pbSpecsNoAns
  (es, pbs) <- partitionEithers <$> runPbs pbSpecsWithAns
  printTestReport (length es) (toPbTest <$> pbs)
  errPbExceptions es
  where (badIds, pbSpecs)              = mkPbSpecs (pbIds cfg)
        (pbSpecsWithAns, pbSpecsNoAns) = partition hasAnswer pbSpecs

-- | Print an test report with a given
-- configuration and a list of problem runs.
printTestReport :: Int -> [PbTest] -> IO ()
printTestReport nException pbTests = do
  when (np /= 0) $
    putColumns [toGreen "passed ", show3d np, showpc (percentage np)]
  when (nf /= 0) $ do
    putColumns [toRed "failed ", show3d nf, showpc (percentage nf)]
    putStrLn horizontalSeparator
    traverse_ printPbTest failures
  where horizontalSeparator = replicate ((7 + 3 + 6) + 2 * length verticalSeparator) '-'
        (passes, failures)  = partition isPbTestOk pbTests
        (np, nf)            = (length passes, length failures + nException)
        percentage n        = fromIntegral n / fromIntegral ntot * 100 where ntot = nf + np

-- | Predicate for problems successfully solved.
isPbTestOk :: PbTest -> Bool
isPbTestOk (PbTest i rep ans) = getResult i rep == ans

-- | Print the data available for a problem.
--
-- Uses ANSI strings for color. Suitable for terminals only.
printPbTest :: PbTest -> IO ()
printPbTest pb@(PbTest i rep ans) = putColumns [pbHeader i, ansStr, resStr, msg]
  where ansStr = toGreen $ show ans
        resStr = color   $ show (getResult i rep)
        msg    = getMessage rep
        color  = if isPbTestOk pb then toGreen else toRed
