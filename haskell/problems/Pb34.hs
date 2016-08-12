module Pb34
  ( pb34
  ) where

import LibProblem

import qualified Data.Array.Unboxed  as A
import qualified Data.Vector.Unboxed as V

nMax :: Int
nMax = 7 * factorial 9

pb34 :: Computation
pb34 = answer s $ show s ++ " Σ of all numbers == to their digits' factorials summed"
  where s = sum [n | n <- [3..nMax], n == sumDigitFactorials n]

sumDigitFactorials :: Int -> Int
sumDigitFactorials n
  | n < 10000 = sdf n
  | otherwise = sdfRem + sumDigitFactorials q
  where (q, r) = n `quotRem` 10000
        sdfRem
          | r >= 1000 = sdf r
          | r >= 100  = sdf r + 1
          | r >= 10   = sdf r + 2
          | otherwise = sdf r + 3

sdf :: Int -> Int
sdf = (sdfVec V.!)
  where sdfVec = V.generate 10000 genSdf
          where genSdf = sum . map fact . show
        fact = (array A.!)
          where array :: A.UArray Char Int
                array = A.listArray ('0', '9') $ take 10 factorials
