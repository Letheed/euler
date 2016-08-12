module Pb77
  ( pb77
  ) where

import LibProblem hiding (primes)
import Data.Numbers.Primes

pb77 :: Computation
pb77 = answer n $ show n ++ " 1st value writable as a Σ of primes in > 5000 ways (" ++ show nWays ++ ")"
  where (n, nWays) = head [(x, nSums) | x <- [2..], let nSums = countSums x, nSums > 5000]


countSums :: Int -> Int
countSums = go primes
  where go _            0     = 1
        go parts@(p:ps) total
          | total < p         = 0
          | otherwise         = subThis + subNext
          where subThis = go parts (total - p)
                subNext = go ps total
