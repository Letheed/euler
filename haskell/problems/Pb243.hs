module Pb243
  ( pb243
  ) where

import LibProblem
import Data.Ratio
import Math.Sieve.Phi

default (Int, Double)

minResilience :: Ratio Int
minResilience = 15499 % 94744

pb243 :: Computation
pb243 = do
  show . head . dropWhile (minResilience <=) $ [totient d % (d-1) | d <- [12..]]
  show "hi"

totient = phi phiSieve

phiSieve = sieve 200000
