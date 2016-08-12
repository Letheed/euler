module Pb86
  ( pb86
  ) where

import LibProblem
import Data.List
import Data.Maybe

goal :: Int
goal = 1000000

mMax :: Int
mMax = 2000

pb86 :: Computation
pb86 = msgAnswer $ show m ++ " (" ++ show n ++ " cuboids)"
  where (m, n) = fromMaybe (error "no solution: too few cuboids computed") $
          find ((goal <) . snd) cuboids

cuboids :: [(Int, Int)]
cuboids = clump . sort $ go pTree
  where cMax = sqrtInt (5*mMax*mMax)
        go (Node (a, b, c) c1 c2 c3)
          | c <= cMax = localCuboids ++ go c1 ++ go c2 ++ go c3
          | otherwise = []
          where (x, y) = (min a b, max a b)
                half = flip quot 2
                localCuboids
                  | x < half (y + 1) = lst1
                  | otherwise        = lst1 ++ lst2
                lst1 = [(y', half x') | p <- [1..mMax `quot` y], let (x', y') = (p * x, p * y)]
                lst2 = [(x', (x'+1) - half (y'+1)) | p <- [1..mMax `quot` x], let (x', y') = (p * x, p * y)]

clump :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
clump []        = []
clump (mn0:mns) = clump' mn0 mns
  where clump' stub     []              = [stub]
        clump' (m1, n1) ((m2, n2):rest)
          | m1 == m2                    = clump' (m1, n) rest
          | otherwise                   = (m1, n1) : clump' (m2, n) rest
          where n = n1 + n2
