module Pb105
  ( pb105
  ) where

import LibIO
import LibProblem

import Batteries
import Control.Arrow
import Data.List
import qualified Data.Text as T

type Set = [Int]

pb105 :: Computation
pb105 = do
  sets <- map parseSet . T.lines <$> readProblemT 105 "sets"
  answer' . sum . map sum . filter isSpecialSumSet $ sets

parseSet :: T.Text -> Set
parseSet = sort . map readIntT . splitListT

isSpecialSumSet :: Set -> Bool
isSpecialSumSet = all respectRules . subsets
  where respectRules (subset1, subset2) = case compare len1 len2 of
          LT -> sum1 < sum2
          EQ -> sum2 /= sum1
          GT -> sum1 > sum2
          where (sum1, sum2) = (sum subset1, sum subset2)
                (len1, len2) = (length subset1, length subset2)

subsets :: Set -> [(Set, Set)]
subsets set = concatMap (\(set1, set2) -> zip (repeat set1) . concatMap (`combinationsOf` set2) $ [length set1..length set2]) pairSets
  where pairSets = map (id &&& (set `minus`)) . concatMap (`combinationsOf` set) $ [len, len-1..2]
        len = length set `quot` 2

minus :: Set -> Set -> Set
minus [] _                  = []
minus xs []                 = xs
minus xs@(x:xs') ys@(y:ys') = case compare x y of
                                LT -> x : minus xs' ys
                                EQ -> minus xs' ys'
                                GT -> minus xs ys'
