module Pb45
  ( pb45
  ) where

import LibProblem

pb45 :: Computation
pb45 = answer d $ show d ++ " 3rd triangular + pentagonal + hexagonal number"
  where d = nextTriPenHex lst1 lst2 lst3
        lst1 = drop 285 $ polygonals 3
        lst2 = drop 165 $ polygonals 5
        lst3 = drop 143 $ polygonals 6

nextTriPenHex :: [Int] -> [Int] -> [Int] -> Int
nextTriPenHex lst1 lst2@(b:bs) lst3@(c:cs)
  | c > b     = nextTriPenHex lst1 bs lst3
  | b > c     = nextTriPenHex lst1 lst2 cs
  | a == b    = a
  | otherwise = nextTriPenHex as bs cs
  where a  = head as
        as = dropWhile (b >) lst1
