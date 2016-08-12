module Pb90
  ( pb90
  ) where

import LibProblem

displayFaces = ["01", "04", "09", "16", "25", "36", "49", "64", "81"]

cubes = filter isValid (combinations 6 ['0'..'9'])
  where isValid cube = all canDisplay displayFaces
          where canDisplay [face1, face2] = face1 `elem` cube || face2 `elem` cube

pb90 :: Computation
pb90 = answer' $ count
  where count = length [() | cube1 <- cubes
                           , cube2 <- dropWhile (cube1 >) cubes
                           , isValidArrangement cube1 cube2]

isValidArrangement cube1 cube2 = all canDisplay displayFaces
  where canDisplay [face1, face2] = arrangement1 || arrangement2
          where arrangement1 = face1 `elem` cube1 && face2 `elem` cube2
                arrangement2 = face1 `elem` cube2 && face2 `elem` cube1

combinations 0 _      = [[]]
combinations _ []     = []
combinations n (x:xs) = withX ++ withoutX
  where withX
          | x == '6' || x == '9' = map ("69" ++) (combinations (n-1) xs)
          | otherwise            = map (x:) (combinations (n-1) xs)
        withoutX = combinations n xs
