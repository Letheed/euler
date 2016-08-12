module LibProblem
  ( module ProblemExports
    -- ** Splits
  , splitListT, splitQuotedListT
    -- ** Reads
  , readIntS, readIntT, readIntegerS, readIntegerT
  ) where

import Computation   as ProblemExports (Computation, answer, answer', msgAnswer)
import Math          as ProblemExports

import Data.Char
import qualified Data.Text as T

-- | Split a `Text` over commas.
splitListT :: T.Text -> [T.Text]
splitListT = T.split (',' ==)

-- | Split a `Text` over commas
-- and drop double-quotes surrounding the elements.
splitQuotedListT :: T.Text -> [T.Text]
splitQuotedListT = map (T.dropAround (== '"')) . splitListT

-- | Parse an `Int` from a `String`.
readIntS :: String -> Int
readIntS []        = error "readIntS: empty string"
readIntS ('-':str) = - (concatDigits . map digitToInt $ str)
readIntS str       =    concatDigits . map digitToInt $ str

-- | Parse an `Int` from a `Text`.
readIntT :: T.Text -> Int
readIntT = readIntS . T.unpack

-- | Parse an `Integer` from a `String`.
readIntegerS :: String -> Integer
readIntegerS []        = error "readIntegerS: empty string"
readIntegerS ('-':str) = - (concatDigits . map (toInteger . digitToInt) $ str)
readIntegerS str       =    concatDigits . map (toInteger . digitToInt) $ str

-- | Parse an `Integer` from a `Text`.
readIntegerT :: T.Text -> Integer
readIntegerT = readIntegerS . T.unpack
