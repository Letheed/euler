module Pb120
  ( pb120
  ) where

import LibProblem

default (Int)

pb120 :: Computation
pb120 = answer' $ sum [a * (a - 2 + a `rem` 2) | a <- [3..1000]]
