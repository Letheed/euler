module Pb93
  ( pb93
  ) where

import LibProblem

import Batteries
import Data.List
import Data.Maybe

default (Int, Double)

pb93 :: Computation
pb93 = do
  let digitSets = combinationsOf 4 [1..9]
  let bestSet = last . sortOn (countConsecutive . positiveExprResults) $ digitSets
  let bestLen = countConsecutive . positiveExprResults $ bestSet
  msgAnswer $ "best digit set: " ++ show bestSet ++ " (" ++ show bestLen ++ " consecutive positive integers)"
  where positiveExprResults = dropWhile (1 >) . exprResults
        countConsecutive xs@(x:_) = length . takeWhile id . zipWith (==) [x..] $ xs
        countConsecutive _        = 0

opSets :: [[Maybe Double -> Maybe Double -> Maybe Double]]
opSets = setsOf 3 [(+$), (-$), (*$), (/$)]
  where Just a +$ Just b = Just (a + b)
        _      +$ _      = Nothing
        Just a -$ Just b = Just (a - b)
        _      -$ _      = Nothing
        Just a *$ Just b = Just (a * b)
        _      *$ _      = Nothing
        Just a /$ Just b = if b == 0 then Nothing else Just (a / b)
        _      /$ _      = Nothing

exprResults :: [Int] -> [Int]
exprResults digitSet = dedup . mapMaybe asIntegral . concatMap applyOpSet $ opSets
  where asIntegral r = let (n, f) = properFraction r in if f == 0 then Just n else Nothing
        applyOpSet [f, g, h] = concatMap (applyParen . map (Just . fromIntegral)) . permutations $ digitSet
          where applyParen [a, b, c, d] = catMaybes [ h d . g c . f b $ a
                                                    , h d . f (g c b) $ a
                                                    , g (h d c) . f b $ a
                                                    , f (h d . g c $ b) a
                                                    , g (h d c) (f b a)
                                                    ]

setsOf  :: (Eq a, Num a) => a -> [t] -> [[t]]
setsOf 0 _  = [[]]
setsOf _ [] = []
setsOf n xs = concatMap (\x -> map (x:) (setsOf (n-1) xs)) xs
