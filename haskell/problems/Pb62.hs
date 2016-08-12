module Pb62
  ( pb62
  ) where

import LibProblem

import Batteries
import Data.List
import Data.Ord
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

pb62 :: Computation
pb62 = answer cube $ show cube ++ " " ++ show cbrt ++ "³ and permutation of cubes " ++ show (drop 1 cbrtSet)
  where cube = cbrt^3
        cbrt = head cbrtSet
        cbrtSet  = minimumBy (comparing head) cbrtSets
        cbrtSets = fmap snd . M.toList . M.filter (lengthIs 5) $ permCubesMap

permCubesMap :: M.Map T.Text [Integer]
permCubesMap = foldr addToCubePerm M.empty [1..10000]
  where addToCubePerm n = M.insertWith (++) (T.pack . sort . show $ n^3) [n]
