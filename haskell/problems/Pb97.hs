module Pb97
  ( pb97
  ) where

import LibProblem
import Math.NumberTheory.Moduli

nDecimals :: Int
nDecimals = 10

pb97 :: Computation
pb97 = answer' $ (1 + 28433 * powerMod 2 (7830457::Int) (10^nDecimals)) `rem` 10^nDecimals
