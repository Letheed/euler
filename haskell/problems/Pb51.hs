module Pb51
  ( pb51
  ) where

import LibProblem

import Batteries
import Data.Array.Unboxed
import Data.Char
import Data.List

wantedLength :: Int
wantedLength = 8

-- Only interesting for 8 and above.
mustSeeDigits :: [Char]
mustSeeDigits = [intToDigit d | d <- [0..10 - wantedLength]]

pb51 :: Computation
pb51 = answer p $ show p ++ " smallest prime part of an eight prime value family"
  where p = head . head . filter (lengthIs wantedLength) . concatMap families $ primes

families :: Int -> [[Int]]
families p
  | all (`notElem` mustSeeDigits) pStr = []
  | otherwise = map mkFamily replaceIdxLst
  where pStr = show p
        size = length pStr
        imax = size - 1
        replaceIdxLst = concatMap ($ [1..imax]) (combinationsOf <$> [1..imax])
        mkFamily = filter isPrime . replace
          where replace idx
                  | last idx == imax = map insertDigit [1..9]
                  | otherwise        = pMask : map insertDigit [1..9]
                  where pMask = foldl' zero p idx
                        insertDigit d = foldl' (\n i -> n + d * powerOf10 i) pMask idx
        zero n i = n - d * powerOf10 i
          where d = (n `quot` powerOf10 i) `rem` 10

powerOf10 :: Int -> Int
powerOf10 = (po10Array !)
  where po10Array :: Array Int Int
        po10Array = listArray (1, 9) [10^i | i <- [1..9::Int]]
