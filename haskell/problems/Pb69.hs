module Pb69
  ( pb69
  ) where

import LibProblem

import Data.List

pb69 :: Computation
pb69 = answer n $ show n ++ " value <= 10⁶ for which n/φ(n) is max"
  where n = last . takeWhile (1000000 >) . scanl' (*) 1 $ primes
