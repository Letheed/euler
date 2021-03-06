{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Computation
  ( Computation
  , PbReport
  , Result(..)
    -- ** Constructors
  , answer, answer', msgAnswer
    -- ** Getters
  , getMResult, getMessage
  ) where

import Control.DeepSeq

-- | Problems are of type `Computation`.
--
-- The `IO` monad is used to preserve access to files, random generators
-- and other side-effectful actions, as well as keep the execution order
-- deterministic.
type Computation = IO PbReport

-- | Report generated by a problem.
data PbReport = PbReport
  { mresult :: Maybe Result  -- ^ Problem's `Result`.
                             -- The `Maybe` wrapper is a temporary compatibility workaround.
  , message :: String        -- ^ A message to display.
  }

-- | Result generated by a problem.
-- Typically a signed integer.
--
-- Used to verify that the answer is correct.
newtype Result = Result Integer
  deriving (Eq, NFData)

instance Show Result where
  show (Result res) = show res
  showsPrec d (Result res) = showsPrec d res


-- | Make a `PbReport` from an integer and a message and lift it to IO.
answer :: (Integral a) => a -> String -> Computation
answer val msg = pure $ PbReport (Just . Result . fromIntegral $ val) msg

-- | Make a `PbReport` from an integer and lift it to IO.
-- A basic message is generated with the passed value.
answer' :: (Integral a, Show a) => a -> Computation
answer' val = answer val (show val)

{-# WARNING msgAnswer "Temporary workaround to make old problems work with the new API. Will be removed eventually. DO NOT USE." #-}
-- | Make a `PbReport` from a message and lift it to IO.
msgAnswer :: String -> Computation
msgAnswer = pure . PbReport Nothing


-- | Getter for the problem's `Result`.
getMResult :: PbReport -> Maybe Result
getMResult = mresult

-- | Getter for the problem's message.
getMessage :: PbReport -> String
getMessage = message
