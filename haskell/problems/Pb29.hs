module Pb29
  ( pb29
  ) where

import LibProblem

import Batteries

pb29 :: Computation
pb29 = answer n $ show n ++ " distinct terms in [a^b | a <- [2..100], b <- [2..100]]"
  where n = length . dedup $ [a^b | a <- [2..100], b <- [2..100]]
