module Pb91
  ( pb91
  ) where

import LibProblem

n :: Int
n = 50

pb91 :: Computation
pb91 = answer' $ count
  where count = 3 * n*n + sum [2 * min ((n-x) * d `quot` y) (y * d `quot` x)
                              | x <- [1..n], y <- [1..n], let d = gcd x y]
