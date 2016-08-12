module Pb43
  ( pb43
  ) where

import LibProblem

import Batteries
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.IntMap.Strict as M

-- d1  d1 /= 0
-- d4  2 | d4
-- d5  3 | d3d4d5 <=> 3 | d3 + d4 + d5
-- d6  d6 = 5 <= 5 | d4d5d6 and 11 | d6d7d8

pb43 :: Computation
pb43 = answer s $ show s ++ " Σ of all subdivisible 0-9 pandigitals"
  where s = sum [toNum [d1, d2, d3, d4, d5, 5, d7, d8, d9, d10]
                | d1 <- ds1, let ds23 = 0 : delete d1 ds1
                , d23@[d2, d3] <- concatMap permutations . combinationsOf 2 $ ds23, let ds4 = ds23 \\ d23
                , d4 <- filter even ds4, let ds5 = delete d4 ds4
                , d5 <- ds5, (d3 + d4 + d5) `rem` 3 == 0, let ds7 = delete d5 ds5
                , d7 <- ds7, toNum [d5, d6, d7] `rem` 7 == 0, let ds8 = delete d7 ds7
                , d8 <- ds8, toNum [d6, d7, d8] `rem` 11 == 0, let ds = delete d8 ds8
                , (d9, d10) <- fromJust $ M.lookup d8 map8910, d9 `elem` ds && d10 `elem` ds
                , toNum [d7, d8, d9] `rem` 13 == 0
                ]
          where d6 = 5
                ds1 = delete d6 [1..9]

-- map of (d9, d10) indexed by d8 so that d8d9d10 is divisible by 17
map8910 :: M.IntMap [(Int, Int)]
map8910 = M.fromList . map teamd8 . groupBy ((==) `on` head) $ sort ds8910
  where ds8910 = filter (\ds -> toNum ds `rem` 17 == 0) . concatMap permutations $ combinationsOf 3 [0..9]
        teamd8 ds910 = (d8, map (\[_,d9,d10] -> (d9, d10)) ds910)
          where d8 = head (head ds910)
