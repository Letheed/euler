module Pb12
  ( pb12
  ) where

import LibProblem

pb12 :: Computation
pb12 = answer tMaxDiv $ show tMaxDiv ++ " with " ++ show (nFactors tMaxDiv) ++ " divisors"
  where tMaxDiv = head . filter ((500 <) . nFactors) $ polygonals 3
