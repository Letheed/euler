module Pb4
  ( pb4
  ) where

import LibProblem

pb4 :: Computation
pb4 = answer n $ show n ++ " largest palindrome Π of 2 3-digit numbers " ++ show xy
  where (n, xy) = maximum [(n, (x, y)) | x <- [999, 998..900::Int]
                                       , y <- [x, x-1..900]
                                       , let n = x * y, isPalindromic n]
