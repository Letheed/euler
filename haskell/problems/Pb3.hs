module Pb3
  ( pb3
  ) where

import LibProblem

pb3 :: Computation
pb3 = answer n $ show n ++ " largest prime factor of 600851475143"
  where n = largestPrimeFactor 600851475143

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = go n 3
  where go n d
          | r == 0    = go q d
          | d * d < n = go n (d + 2)
          | otherwise = n
          where (q, r) = n `quotRem` d
