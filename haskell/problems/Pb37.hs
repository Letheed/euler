module Pb37
  ( pb37
  ) where

import LibProblem

pb37 :: Computation
pb37 = answer s $ show s ++ " Σ of the 11 rightward and leftward truncatable primes"
  where s = sum $ take 11 [p | p <- drop 4 primes, isTruncatablePrime p]

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = isTruncatable 10 (n `quot` 10)
  where isTruncatable _     0 = True
        isTruncatable order m = isPrime m && isPrime n' && isTruncatable (order * 10) (m `quot` 10)
          where n' = n - m * order
