module Pb72
  ( pb72
  ) where

import LibProblem

import Data.List

uBound :: Int
uBound = 1000000

pb72 :: Computation
pb72 = answer s $ show s
  where s = sum $ phi <$> [2..uBound]

phi :: Int -> Int
phi = product . map phiPow . group . primeFactors
  where phiPow ~ds@(d:_) = d^(length ds - 1) * (d - 1)
