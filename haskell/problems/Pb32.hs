module Pb32
  ( pb32
  ) where

import LibProblem

import Batteries
import Data.List

pb32 :: Computation
pb32 = answer s $ show s ++ " Σ of all pandigital product identities"
  where s  = sum [c | c <- cs , isPandigitalProduct c]
        cs = map toNum . concatMap permutations . combinationsOf 4 $ [1..9]

isPandigitalProduct :: Int -> Bool
isPandigitalProduct c = any (\(a, b) -> abStr == sort (show a ++ show b)) factorPairs
  where cStr  = show c
        abStr = ['1'..'9'] \\ cStr
        factorPairs = [(a, b) | a <- [2..sqrtInt c], let (b, r) = c `quotRem` a, r == 0]
