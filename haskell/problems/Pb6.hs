module Pb6
  ( pb6
  ) where

import LibProblem

pb6 :: Computation
pb6 = answer' $ sum [1..100::Int]^(2::Int) - sum (map (^(2::Int)) [1..100])
