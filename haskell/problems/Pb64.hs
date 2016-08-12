module Pb64
  ( pb64
  ) where

import LibProblem

import Data.Maybe

pb64 :: Computation
pb64 = answer l $ show l ++ " continued fractions of √N have an odd period (N ≤ 10⁴)"
  where l = length . filter odd . mapMaybe period $ [2..10000]

period :: Double -> Maybe Int
period x
  | y * y == x = Nothing  -- x is a perfect square
  | otherwise  = Just $ go 0 0 1 y
  where y = truncToFloat . sqrt $ x
        truncToFloat = fromIntegral . (truncate :: Double -> Int)
        go n m d a
          | a == 2 * y = n  -- periodicity
          | otherwise  = go (n+1) m' d' a'
          where m' = d * a - m
                d' = (x - m' * m') / d
                a' = truncToFloat $ (y + m') / d'
