{-# LANGUAGE MultiWayIf, TupleSections #-}

module Pb26
  ( pb26
  ) where

import LibProblem

import Data.List
import Data.Maybe

pb26 :: Computation
pb26 = case maxCycleLen [999, 998..0] of
  Nothing       -> answer 0 "No cycle found"
  Just (len, n) -> answer n $ show n ++ " has the longest recurring cycle (" ++ show len ++ " decimals)"

maxCycleLen :: [Int] -> Maybe (Int, Int)
maxCycleLen = go Nothing
  where go sol (n:ns) = case sol of
          Nothing       -> go sol' ns
          Just (len, _) -> if | len > n   -> sol
                              | otherwise -> go (max sol sol') ns
          where sol' = (, n) <$> cycleLen n
        go sol []     = sol

cycleLen :: Int -> Maybe Int
cycleLen n
  | null dec  = Nothing
  | otherwise = listToMaybe [len | (len, dec') <- decTails, hasCycle dec dec']
  where dec = drop n $ unitFracDecimals n
        decTails = zip [1..] . take n . tails . drop 1 $ dec
        hasCycle ds ds' = all (uncurry (==)) . take n $ zip ds ds'

unitFracDecimals :: Int -> [Int]
unitFracDecimals n = go 10
  where go m
          | m == 0    = []
          | otherwise = (m `quot` n) : go (10 * (m `rem` n))
