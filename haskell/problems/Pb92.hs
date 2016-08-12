module Pb92
  ( pb92
  ) where

import LibProblem
import Data.Array.Unboxed as UA

uBound :: Int
uBound = 10000000

pb92 :: Computation
pb92 = answer' $ count
  where count = length . filter isAn89Chain $ [2..uBound-1]

squares :: UA.Array Char Int
squares = listArray ('0', '9') [n*n | n <- [0..9]]

isAn89Chain :: Int -> Bool
isAn89Chain n = isAn89ChainArray ! next
  where next = sum . fmap (squares UA.!) . show $ n

isAn89ChainArray :: UArray Int Bool
isAn89ChainArray = listArray (1, nextMax) (map isAn89Chain [1..nextMax])
  where nextMax = (9 * 9) * ceiling (logBase 10.0 (fromIntegral uBound))
        isAn89Chain n
          | n == 89   = True
          | n == 1    = False
          | otherwise = isAn89Chain next
          where next = sum . fmap (squares UA.!) . show $ n
