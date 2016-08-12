module Pb7
  ( pb7
  ) where

import LibProblem

pb7 :: Computation
pb7 = answer n $ show n ++ " 10001st prime number"
  where n = primes !! 10000
