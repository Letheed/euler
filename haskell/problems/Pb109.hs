module Pb109
  ( pb109
  ) where

import LibProblem

type Throw = [Dart]

data Multiplier = Single
                | Double
                | Treble

data Dart = Dart Multiplier Int

regions :: [Int]
regions = [1..20]

bullRegion :: [Int]
bullRegion = [25]

pb109 :: Computation
pb109 = answer' . length . filter (100 >) . map scoreThrow $ checkOutThrows

scoreThrow :: Throw -> Int
scoreThrow = sum . map scoreDart

scoreDart :: Dart -> Int
scoreDart (Dart multiplier score) = case multiplier of
  Single -> score
  Double -> score * 2
  Treble -> score * 3

checkOutThrows :: [Throw]
checkOutThrows = oneDartThrows ++ twoDartThrows ++ threeDartThrows
  where lastDarts = map (Dart Double) (regions ++ bullRegion)
        allDarts  = regionDarts ++ bullDarts
          where regionDarts = concatMap ([Dart Single, Dart Double, Dart Treble] <*>) (pure <$> regions)
                bullDarts   = [Dart Single, Dart Double] <*> bullRegion
        oneDartThrows   = map (:[]) lastDarts
        twoDartThrows   = concatMap (\dart -> map (dart:) oneDartThrows) allDarts
        threeDartThrows = concatMap (\darts -> map (darts ++) oneDartThrows) twoDarts
          where twoDarts = concatMap (\ds -> map (:[head ds]) ds) . tails $ allDarts

tails :: [a] -> [[a]]
tails []  = []
tails lst = lst : tails (tail lst)
