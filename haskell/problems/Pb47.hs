module Pb47
  ( pb47
  ) where

import LibProblem

import Data.Maybe

pb47 :: Computation
pb47 = answer n $ show n ++ " 1st of the 1st 4 consecutive ints with 4 distinct prime factors"
  where n = head . cons4PF 4 $ [1..]

cons4PF :: Int -> [Int] -> [Int]
cons4PF n = go
  where go lst = fromMaybe (take n lst) (test n lst)
        test 0 _               = Nothing
        test m (x:xs)
          | nDistinctPF x == n = test (m-1) xs
          | otherwise          = Just $ go xs
