{-# LANGUAGE TupleSections #-}

module Pb38
  ( pb38
  ) where

import LibProblem

import Data.Array.Unboxed
import Data.List
import Data.Maybe

pb38 :: Computation
pb38 = answer pan $ show pan ++ " largest pandigital by concatenation of multiples (" ++ show n ++ ")"
  where (pan, n) = maximum . mapMaybe (\n -> (,n) <$> panMultiple n) $ [9234..9488]

panMultiple :: Int -> Maybe Int
panMultiple n = find isPandigital . takeWhile (987654321 >=) . scanl concatMultiples n $ [2..]
  where concatMultiples acc k = acc * order multiple + multiple
          where multiple = n * k
                order m = 10 * if m < 10 then 1 else order (m `quot` 10)

isPandigital :: Int -> Bool
isPandigital n = n >= 123456789 && digits ! '0' == 0 && all ((1 ==) . (digits !)) ['1'..'9']
  where digits :: UArray Char Int
        digits  = accumArray (\n _ -> n + 1) 0 ('0', '9') [(i, ()) | i <- show n]
