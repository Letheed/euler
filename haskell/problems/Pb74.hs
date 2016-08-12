module Pb74
  ( pb74
  ) where

import LibProblem
import Data.Vector as Vec(Vector, (!), generate)
import Data.Vector.Unboxed as UVec(Vector, (!), generate)

type Vec  = Vec.Vector
type UVec = UVec.Vector

uBound :: Int
uBound = 1000000

pb74 :: Computation
pb74 = answer' . length . filter ((== 60) . chainLength) $ [2..uBound-1]
  where chainLength = (chainLengthVec Vec.!)

chainLengthVec :: Vec Int
chainLengthVec = Vec.generate uBound chainLength
  where chainLength n
          | n `elem` [2, 145, 40585]          = 1 -- [0, 1] too, but not needed.
          | n `elem` [169, 1454, 363601]      = 3
          | n `elem` [871, 872, 45361, 45362] = 2
          | otherwise                         = 1 + lengthRest
          where lengthRest
                  | next' < uBound = chainLengthVec Vec.! next'
                  | otherwise      = 1 + chainLength (next next')
                  where next' = next n

next :: Int -> Int
next n = sum $ factorial <$> digits n
  where factorial = (factorialVec UVec.!)
        digits a = a `rem` 10 : go (a `quot` 10)
          where go b
                  | b == 0    = []
                  | otherwise = b `rem` 10 : go (b `quot` 10)

factorialVec :: UVec Int
factorialVec = UVec.generate 10 fac
  where fac n = product [1..n]
