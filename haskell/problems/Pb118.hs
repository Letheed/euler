module Pb118
  ( pb118
  ) where

import LibProblem hiding (isPrime)

import Batteries
import Data.List
import Data.Maybe
import Data.Numbers.Primes

import qualified Data.Map.Strict as M

pb118 :: Computation
pb118 = msgAnswer $ show $ length panPrimeSets

panPrimeSets :: [[Int]]
panPrimeSets = concatMap (primeSetsFrom . map (fromJust . (`M.lookup` panDigitPrimeMap))) panDigitSets
  where digitSets    = M.keys panDigitPrimeMap
        panDigitSets = go [] digitSets
          where go _   []        = []
                go set (ds:rest) = setsWithds ++ go set rest
                  where setsWithds
                          | isPanSet set' = [set']
                          | null rest'    = []
                          | otherwise     = go set' rest'
                          where set'  = ds : set
                                rest' = filter (null . intersect ds) rest
                                isPanSet = ([1..9] ==) . sort . concat
        primeSetsFrom = go []
          where go set []        = [set]
                go set (ps:rest) = concatMap (\p -> go (p:set) rest) ps

panDigitPrimeMap :: M.Map [Int] [Int]
panDigitPrimeMap = M.fromList . filter (some . snd) . map numbersFromSet $ digitSets
  where digitSets = concatMap (`combinationsOf` [1..9]) [1..8] -- sum [1..9] == 45 is divisible by 3.
                                                               -- so there is no 9-digit pandigital prime.
        numbersFromSet ds = (ds, filter isPrime . map concatDigits . permutations $ ds)
        concatDigits = foldl1' (\n d -> n * 10 + d) -- doubles with concatDigits from Math
