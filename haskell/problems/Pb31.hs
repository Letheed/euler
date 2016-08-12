{-# LANGUAGE MagicHash #-}

module Pb31
  ( pb31
  ) where

import LibProblem

import GHC.Exts

englishCoins :: [Int]
englishCoins = [200, 100, 50, 20, 10, 5, 2, 1]

pb31 :: Computation
pb31 = answer ways $ show ways ++ " ways to make £2 using english coins"
  where ways = sumWays 200

sumWays :: Int -> Int
sumWays (I# wanted) = I# (go 0# englishCoins)
  where go _     []             = 0#
        go total cs@(I# c:rest) = sumThis +# sumNext
          where sumThis
                  | isTrue# (sum' >#  wanted) = 0#
                  | isTrue# (sum' ==# wanted) = 1#
                  | otherwise                 = go sum' cs
                  where sum' = total +# c
                sumNext = go total rest :: Int#
