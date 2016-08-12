module Pb35
  ( pb35
  ) where

import LibProblem

pb35 :: Computation
pb35 = answer n $ show n ++ " circular primes < 10⁶"
  where n = length [p | p <- takeWhile (1000000 >) primes, isCircularPrime p]

isCircularPrime :: Int -> Bool
isCircularPrime p = go nRotations p
  where nRotations = numLen p - 1
        go 0 _  = True
        go n p = isPrime pRot && go (n-1) pRot
          where (q, r) = p `quotRem` 10
                pRot = q + r * 10^nRotations
