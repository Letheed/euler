module Pb9
  ( pb9
  ) where

import LibProblem

import Control.Applicative

pb9 :: Computation
pb9 = case findTriplet pTree of
  Nothing        -> answer 0 "no Pythagorean triplet for which a + b + c = 10³ was found"
  Just (a, b, c) -> answer p $ show (a, b, c) ++ " Pythagorean triplet for which a + b + c = 10³"
    where p = a * b * c

findTriplet :: PTree -> Maybe (Int, Int, Int)
findTriplet (Node (a, b, c) c1 c2 c3)
  | r == 0    = Just (a * q, b * q, c * q)
  | s < 1000  = findTriplet c1 <|> findTriplet c2 <|> findTriplet c3
  | otherwise = Nothing
  where s = a + b + c
        (q, r) = 1000 `quotRem` s
