module Pb100
  ( pb100
  ) where

import LibProblem

default (Int)

pb100 :: Computation
pb100 = answer' . fst . head . dropWhile ((nmin >=) . snd) $ solutions
  where nmin = 10^12
        solutions = iterate next s0
          where s0 = (15, 21)
                next (b, n) = (3*b + 2*n - 2, 4*b + 3*n - 3)
