module Pb56
  ( pb56
  ) where

import LibProblem

import Data.List
import Data.Ord

pb56 :: Computation
pb56 = answer s $ show s ++ " maximum digit sum of a^b for a=" ++ show a ++ ", b=" ++ show b
  where (s, (a, b)) = maximumBy (comparing fst) [(digitSum (a^b), (a, b)) | a <-[80..99::Integer], b <-[80..99]]
