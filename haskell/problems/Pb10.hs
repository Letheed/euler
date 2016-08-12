module Pb10
  ( pb10
  ) where

import LibProblem

pb10 :: Computation
pb10 = answer s $ show s ++ " Σ of all primes < 2x10⁶"
  where s = sum . takeWhile (2000000 >) $ primes
