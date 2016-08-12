module Pb2
  ( pb2
  ) where

import LibProblem

pb2 :: Computation
pb2 = answer s $ show s ++ " Σ of the even Fibonacci terms < 4x10⁶"
  where s = sum . filter even . takeWhile (<= 4000000) $ fibs
