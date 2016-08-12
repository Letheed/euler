module Pb65
  ( pb65
  ) where

import LibProblem

pb65 :: Computation
pb65 = answer s $ show s ++ " e's continued fraction 100th convergent's numerator digit sum"
  where s = digitSum $ numerators !! 99
        numerators = go (0::Int) 2 1 1 :: [Integer]
          where go n mul x y
                  | n == 2    = z' : go 0 (mul + 2) y z'
                  | otherwise = z  : go (n + 1) mul y z
                  where z  = x + y
                        z' = x + mul * y
