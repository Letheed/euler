module Pb1
  ( pb1
  ) where

import LibProblem

pb1 :: Computation
pb1 = answer s $ show s ++ " Σ of all multiples of 3 or 5 < 10³"
  where s = sum [x | x <- [1..999::Int], x `mod` 3 == 0 || x `mod` 5 == 0]
