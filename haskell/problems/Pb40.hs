module Pb40
  ( pb40
  ) where

import LibProblem

import Data.Char

pb40 :: Computation
pb40 = answer p $ show p ++ " Π of the digits " ++ show ds
  where ds = wantedDigits 7
        p = product ds

wantedDigits :: Int -> [Int]
wantedDigits = go 1 champernowne
  where go m champer n
          | n == 1    = [d]
          | otherwise = d : go m' champer' (n - 1)
          where d        = digitToInt (head champer)
                m'       = 10 * m
                champer' = drop (m' - m) champer

champernowne :: String
champernowne = concatMap show [(1::Int)..]
