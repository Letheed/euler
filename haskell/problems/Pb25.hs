module Pb25
  ( pb25
  ) where

import LibProblem

import Data.List

pb25 :: Computation
pb25 = case findIndex (\x -> 10^999 <= x && x < 10^1000) (fibs :: [Integer]) of
  Nothing -> answer 0 "no 1000-digit Fibonacci term was found"
  Just i  -> answer i $ show i ++ " index of the first 1000-digit Fibonacci term"
