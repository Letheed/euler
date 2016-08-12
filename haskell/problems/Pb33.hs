module Pb33
  ( pb33
  ) where

import LibProblem

import Data.Ratio

pb33 :: Computation
pb33 = answer' d
  where d = denominator . product $ fractions

fractions :: [Ratio Int]
fractions = [ fraction1 | a1 <- [1..9], a0 <- [a1..9], b0 <- [a1+1..9]
                                  , let fraction1 = (a1*10 + a0) % (a0*10 + b0)
                                  , let fraction2 = a1 % b0
                                  , fraction1 == fraction2
                                  ]
