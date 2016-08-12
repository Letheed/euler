module Pb98
  ( pb98
  ) where

import LibIO
import LibProblem

import Batteries
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M

default (Int)

pb98 :: Computation
pb98 = do
  dict <- readLst <$> readProblem 98 "words"
  let anagrams = groupAnagrams dict
  let squares  = groupOn numLen [n*n | n <- [0..sqrtInt (10^(lenMax + 1))]]
        where lenMax  = length . head . head . last $ anagrams
  let sizeSets = assort anagrams squares
  let pairSets = filter some . map squareAnagramPairs $ sizeSets
  msgAnswer $ show . maximumBy (comparing (snd . snd)) . map (fmap (maximumBy (comparing snd))) . last $ pairSets

groupAnagrams :: [String] -> [[[String]]]
groupAnagrams = filter some . map (filter (lengthGT 1) . sortGroupOn sort) . sortGroupOn length

assort :: [[[String]]] -> [[Int]] -> [([[String]], [Int])]
assort (a:as) (sq:sqs)
  | anaLen == sqLen = (a, sq) : assort as sqs
  | anaLen > sqLen  = assort (a:as) sqs
  | otherwise       = error "missing squares"
  where anaLen = length . head . head $ a
        sqLen  = numLen . head $ sq
assort []     _     = []
assort _      []    = error "missing squares"

squareAnagramPairs :: ([[String]], [Int]) -> [((String, String), [(Int, Int)])]
squareAnagramPairs (anagramLists, squares) = concatMap sqAnaPairs anagramLists
  where sqAnaPairs = mapMaybe sqAnaPair . combinationsOf 2
        sqAnaPair [ana1, ana2]
          | null solutions = Nothing
          | otherwise      = Just ((ana1, ana2), dedup solutions)
          where solutions = mapMaybe trySquare squares
                sqAnaMap = anaMap ana1 ana2
                trySquare sq
                  | isMatch ana1 (show sq) && anaSq `elem` squares = Just (smallSq, bigSq)
                  | otherwise                               = Nothing
                  where anaSq   = readIntS . sqAnaMap . show $ sq
                        smallSq = min sq anaSq
                        bigSq   = max sq anaSq

anaMap :: String -> String -> String -> String
anaMap ana1 ana2 = order ((order `on` (`order` [0..])) ana1 ana2)
  where order orderMap = map snd . sortBy (comparing fst) . zip orderMap

isMatch :: String -> String -> Bool
isMatch ana sq = oneLetterPerDigit && oneDigitPerLetter
  where oneLetterPerDigit = all (alone . snd) . M.toList $ mkMap sq ana
        oneDigitPerLetter = all (alone . snd) . M.toList $ mkMap ana sq
        mkMap lst1 = M.map dedup . M.fromListWith (++) . zip lst1 . map (:[])
