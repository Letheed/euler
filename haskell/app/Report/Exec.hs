{-# LANGUAGE TypeFamilies, ViewPatterns #-}

module Report.Exec
  ( ExecReport(Exec)
  ) where

import Args
import Clock
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
import Data.Maybe
import Data.Monoid
import Data.Ord

-- | Execution report.
data ExecReport = Exec

-- | Configuration for execution reports.
type ExecConfig = Config ExecReport

instance Command ExecReport where
  cmdName = const "exec"

instance ReportCmd ExecReport where
  report = execReport

instance Configurable ExecReport where
  data Config ExecReport = ExecConfig
    { pbIds            :: PbIds
    , sorted           :: Bool
    , sortType         :: Maybe DurationType
    , showedDurations  :: [DurationType]
    , durTypeDefault   :: DurationType
    }
  defaultConfig = ExecConfig
    { pbIds           = DefaultIds
    , sorted          = False
    , sortType        = Nothing
    , showedDurations = [Thread]
    , durTypeDefault  = Thread
    }
  parseOption cfg (Option opt mval)
    | opt `elem` ["--sort", "-s"] = configSort cfg mval
    | opt `elem` ["--time", "-t"] = configTime cfg mval
    | otherwise                   = Nothing
  parseArg cfg arg = updateCfg . ListIds <$> parsePbIdS arg
    where updateCfg newPbIds = cfg { pbIds = pbIds cfg <> newPbIds }

-- | Get the default clock type to use for sorting.
sortDefault :: ExecConfig -> DurationType
sortDefault cfg = fromMaybe (durTypeDefault cfg) (headM (showedDurations cfg))

-- | Set the sort flag to `True`.
setSorted :: ExecConfig -> ExecConfig
setSorted cfg = cfg { sorted = True }

-- | Set the clock type used for sorting.
-- Returns `Nothing` if the option's value is not supported.
configSort :: ExecConfig -> Maybe String -> Maybe ExecConfig
configSort (setSorted -> cfg) = go
  where go  Nothing        = Just cfg { sortType = Nothing }
        go (Just "thread") = Just cfg { sortType = Just Thread }
        go (Just "proc"  ) = Just cfg { sortType = Just Process }
        go  _              = Nothing

-- | Set the clock types used to display execution times.
-- Returns `Nothing` if the option's value is not supported.
configTime :: ExecConfig -> Maybe String -> Maybe ExecConfig
configTime cfg = go
  where go  Nothing                                = Just cfg { showedDurations = [durTypeDefault cfg] }
        go (Just "none")                           = Just cfg { showedDurations = [] }
        go (Just (parseDurTypes -> Just durTypes)) = Just cfg { showedDurations = nub durTypes }
        go  _                                      = Nothing
        parseDurTypes = foldr parseDurType (Just []) . splitOn ','
          where parseDurType "thread" mDurTypes = (Thread  :) <$> mDurTypes
                parseDurType "proc"   mDurTypes = (Process :) <$> mDurTypes
                parseDurType _        _         =  Nothing

-- | Generate an execution report with a given configuration.
-- The problems specified in the configuration are run and
-- a report of their results and performance is printed.
execReport :: ExecConfig -> IO ()
execReport cfg = do
  warnPbNotFound badIds
  pbRuns <- runPbs pbSpecs
  printExecReport cfg pbRuns
  errPbExceptions (lefts pbRuns)
  where (badIds, pbSpecs) = mkPbSpecs (pbIds cfg)

-- | Print an execution report with a given
-- configuration and a list of problem runs.
printExecReport :: ExecConfig -> [PbRun] -> IO ()
printExecReport cfg pbRuns = do
  traverse_ putPbRun sortedPbRuns
  when (several pbRuns) $ do
    putStrLn horizontalSeparator
    putStrLn totalLine
    putStrLn averageLine
  where nProblems           = length pbRuns
        sortedPbRuns        = sortPbRuns cfg pbRuns
        cfgDurations        = showedDurations cfg
        showCfgDurations    = showDurations cfgDurations
        timeSpecsTotal      = mconcat $ getDurations <$> rights pbRuns
        putPbRun            = putStrLn . showPbRun cfg
        horizontalSeparator = replicate ((7 + 3 + 6 * nDurCols + 3) + lenSeparators) '-'
          where nDurCols      = length cfgDurations
                lenSeparators = (nDurCols + 2) * length verticalSeparator
        totalLine           = columns ["total  ", show3d nProblems, columns durTotals, errStatus]
          where durTotals     = showCfgDurations timeSpecsTotal
                errStatus     = toErrStatus $ allPbRunOk pbRuns
        averageLine         = columns ["average", replicate 3 ' ', columns durAverages]
          where durAverages   = showCfgDurations (timeSpecsTotal `quotTs` nProblems)

-- | Show the report of a problem run.
showPbRun :: ExecConfig -> PbRun -> String
showPbRun cfg = either (showPbException cfg) (showPb cfg)

-- | Show the report for a problem that ran properly.
showPb :: ExecConfig -> Problem -> String
showPb cfg pb@(Pb i _ rep mAns) = columns [pbHeader i, columns durs, errStatus, getMessage rep]
  where durs      = showDurations (showedDurations cfg) (getDurations pb)
        errStatus = maybe "---" (toErrStatus . (getResult i rep ==)) mAns

-- | Show a report for a problem that threw an exception.
showPbException :: ExecConfig -> PbException -> String
showPbException cfg (PbEx i e) = columns [pbHeader i, columns durs, errStatus, msg]
  where durs      = const "   N/A" <$> showedDurations cfg
        errStatus = toErrStatus False
        msg       = toRed "exception raised" ++ maybe "" (": " ++) (headM . lines . show $ e)

-- | Sort a list of problem runs according to the configuration.
sortPbRuns :: ExecConfig -> [PbRun] -> [PbRun]
sortPbRuns cfg pbRuns
  | sorted cfg = (fmap Right . sortPbs cfg $ rights pbRuns) ++ filter isLeft pbRuns
  | otherwise  = pbRuns

-- | Sort a list of problems according to the configuration.
sortPbs  :: ExecConfig -> [Problem] -> [Problem]
sortPbs cfg
  | sorted cfg = sortBy (flip (comparing (getDuration durType)))
  | otherwise  = id
  where durType = fromMaybe (sortDefault cfg) (sortType cfg)
