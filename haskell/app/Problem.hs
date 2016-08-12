{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms, GeneralizedNewtypeDeriving #-}

module Problem
  ( PbIds(DefaultIds, ListIds)
  , PbSpec, mkPbSpecs, hasAnswer
  , PbRun
  , Problem(Pb), getDuration, getDurations
  , runPbs
  , isPbOk, allPbOk, isPbRunOk, allPbRunOk
  , PbException(PbEx)
  , warnPbNotFound, warnPbHasNoAnswer
  , errPbException, errPbExceptions
  ) where

import Clock
import Present
import Problems

import Batteries
import Control.Arrow
import Control.DeepSeq
import Control.Exception.Base
import Data.Foldable
import Data.Maybe
import System.IO
import qualified Data.IntMap.Lazy as Map

import GHC.OverloadedLabels


-- | Correct answer to a problem.
-- `Nothing` if the answer is unknown.
type MAnswer  = Maybe Result

-- | Data resulting from a problem run.
-- `Problem` if the run succeeded
-- or `PbException` if an exception was raised.
type PbRun    = Either PbException Problem


-- | IDs of the problems to run.
newtype PbIds = PbIds (Maybe [PbId])
              deriving (Monoid)

-- | No IDs were specified.
pattern DefaultIds :: PbIds
pattern DefaultIds = PbIds Nothing

-- | A list of specific IDs to run.
pattern ListIds :: [PbId] -> PbIds
pattern ListIds l  = PbIds (Just l)


-- | Specifications of a problem.
-- Contains the data related to a problem prior to running it.
data PbSpec = PbSpec
  { pbId         :: PbId
  , computation  :: Computation
  , mAnswer      :: MAnswer
  }

instance IsLabel "pbId"    (PbSpec -> PbId)    where fromLabel _ = pbId
instance IsLabel "mAnswer" (PbSpec -> MAnswer) where fromLabel _ = mAnswer

-- | Constructor for `PbSpec`.
-- Makes a `PbSpec` from the ID and the computation.
-- Retrieves the answer if it exists.
mkPbSpec :: (PbId, Computation) -> PbSpec
mkPbSpec (i, comput) = PbSpec i comput (lookupId answerMap i)

-- | Make `PbSpec`s for a bunch of IDs.
-- Also returns a list of the IDs that don't match a known problem.
-- If no ID was specified, then all known problems are used.
mkPbSpecs :: PbIds -> ([PbId], [PbSpec])
mkPbSpecs  DefaultIds     = ([], pbSpecsAll)
mkPbSpecs (ListIds pbIds) = mkPbSpecsIds pbIds

-- | Make `PbSpec`s for a list of IDs.
-- Also returns a list of the IDs that don't match a known problem.
mkPbSpecsIds :: [PbId] -> ([PbId], [PbSpec])
mkPbSpecsIds pbIds = (badIds, mkPbSpec <$> computs)
  where (badIds, computs) = lookupIds problemMap pbIds

-- | `PbSpec`s for all known problems.
pbSpecsAll :: [PbSpec]
pbSpecsAll = mkPbSpec . first mkPbId' <$> Map.toList problemMap
  where mkPbId'  = fromMaybe throwErr . mkPbId
        throwErr = error "pbSpecsAll: pbId out of range in problemMap"

-- | Predicate for problems with a known answer.
hasAnswer :: PbSpec -> Bool
hasAnswer = isJust . ((#mAnswer) :: PbSpec -> MAnswer)


-- | Problem data.
-- Contains the data related to a problem after running it,
-- including performance measurements.
data Problem = Problem
  { pbId       :: PbId
  , durations  :: TimeSpecs
  , pbReport   :: PbReport
  , mAnswer    :: MAnswer
  }

instance IsLabel "pbId"    (Problem -> PbId)    where fromLabel _ = pbId
instance IsLabel "mAnswer" (Problem -> MAnswer) where fromLabel _ = mAnswer

-- | Getter pattern for `Problem`.
pattern Pb :: PbId -> TimeSpecs -> PbReport -> MAnswer -> Problem
pattern Pb i durs rep mAns <- Problem i durs rep mAns

-- | Get a specific clock record from a problem.
getDuration :: DurationType -> Problem -> TimeSpec
getDuration durType = selectDur durType . durations

-- | Getter for the time record.
getDurations :: Problem -> TimeSpecs
getDurations = durations

-- | Run a list of problems.
runPbs :: [PbSpec] -> IO [PbRun]
runPbs pbSpecs = do
  pbs <- traverse runPb pbSpecs
  hFlush stdout
  pure pbs

-- | Run a problem.
-- Times the execution and catches exceptions.
runPb :: PbSpec -> IO PbRun
runPb (PbSpec i comput mAns) = handle (pure . Left . PbException i) $ do
  printRunningHeader "running" i
  ts0 <- getTimeSpecs
  rep <- comput
  strictEval rep
  ts1 <- getTimeSpecs
  pure $ Right (Problem i (diffTs ts1 ts0) rep mAns)
  where strictEval report = case getMResult report of
          Nothing  -> evaluate $ rnf (getMessage report)
          Just res -> evaluate $ rnf res

-- | Correctness predicate for problems.
-- `True` if the result and the answer are equal,
-- or if the answer is not known.
isPbOk :: Problem -> Bool
isPbOk pb = maybe True (getResult (#pbId pb) (pbReport pb) ==) (#mAnswer pb)

-- | Correctness predicate for a list of problems.
allPbOk :: [Problem] -> Bool
allPbOk = all isPbOk

-- | Correctness predicate for problem runs.
-- `False` if a exception was raised.
isPbRunOk :: PbRun -> Bool
isPbRunOk = either (const False) isPbOk

-- | Correctness predicate for a list of problem runs.
allPbRunOk :: [PbRun] -> Bool
allPbRunOk = all isPbRunOk

-- | A problem that raised an exception
-- when the computation was executed.
data PbException = PbException
  { pbId       :: PbId
  , exception  :: SomeException
  }

-- | Getter pattern for `PbException`.
pattern PbEx :: PbId -> SomeException -> PbException
pattern PbEx i e <- PbException i e

-- | Print a warning for a list of IDs that do not match known problems.
warnPbNotFound :: [PbId] -> IO ()
warnPbNotFound []     = pure ()
warnPbNotFound [pbId] = putWarnS $ "problem not found: " ++ show pbId
warnPbNotFound pbIds  = putWarnS $ "problems not found: " ++ show (dedup pbIds)

-- | Print a warning for a list of `PbSpec`s that do not have an known answer.
warnPbHasNoAnswer :: [PbSpec] -> IO ()
warnPbHasNoAnswer []   = pure ()
warnPbHasNoAnswer [pb] = putWarnS $ "ignored problem with no answer: " ++ show ((#pbId pb) :: PbId)
warnPbHasNoAnswer pbs  = putWarnS $ "ignored problems with no answer: " ++ show (dedup (map #pbId pbs) :: [PbId])

-- | Print an error for a problem exception.
-- The exception is printed.
errPbException :: PbException -> IO ()
errPbException (PbException i e) = do
  putErrS   $ "exception raised in problem " ++ show i ++ ":"
  ePutStrLn $ show e

-- | Print an error for a list of problem exceptions.
-- The exceptions are printed.
errPbExceptions :: [PbException] -> IO ()
errPbExceptions = traverse_ ((ePutStrLn "" >>) . errPbException)
