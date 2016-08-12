{-# LANGUAGE ScopedTypeVariables #-}

module Pb13
  ( pb13
  ) where

import LibIO
import LibProblem

import qualified Data.Text as T

pb13 :: Computation
pb13 = do
  numbers <- map readIntegerT . T.lines <$> readProblemT 13 "numbers"
  let digits = take 10 . show . sum $ numbers
  answer (readIntS digits) $ digits ++ " are the first 10 digits of the sum"
