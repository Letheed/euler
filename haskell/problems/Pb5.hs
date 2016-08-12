module Pb5
  ( pb5
  ) where

import LibProblem

pb5 :: Computation
pb5 = answer n $ show n ++ " is evenly divisible by all numbers from 1 to 20"
  where n = foldr1 lcm [1..20::Int]
