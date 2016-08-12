module Pb89
  ( pb89
  ) where

import LibIO
import LibProblem
import Data.List
import Data.Maybe

type Roman = [RomanChar]

data RomanChar = I | V | X | L | C | D | M
               deriving (Bounded, Enum, Eq, Ord, Show)

pb89 :: Computation
pb89 = do
  romans <- map parseRoman . lines <$> readProblem 89 "romans"
  let idealRomans = map (toRoman . toDecimal) romans
  answer' $ sum [length roman - length idealRoman | (roman, idealRoman) <- zip romans idealRomans]

toRoman :: Int -> Roman
toRoman n
  | n == 0    = []
  | otherwise = rcs ++ toRoman (n - d)
  where (d, rcs) = fromMaybe (error "toRoman: negative number") $
          find ((<= n) . fst) [(1000, [M]), (900, [C, M]), (500, [D]), (400, [C, D])
                              , (100, [C]), (90, [X, C]), (50, [L]), (40, [X, L])
                              , (10, [X]), (9, [I, X]), (5, [V]), (4, [I, V])
                              , (1, [I])]

toDecimal :: Roman -> Int
toDecimal []            = 0
toDecimal [rc]          = dec rc
toDecimal (rc1:rc2:rcs) = n + toDecimal (rc2:rcs)
  where n
          | rc1 < rc2 = - (dec rc1)
          | otherwise = dec rc1

parseRoman :: String -> Roman
parseRoman = map rom

rom :: Char -> RomanChar
rom c = case c of
  'I' -> I
  'V' -> V
  'X' -> X
  'L' -> L
  'C' -> C
  'D' -> D
  'M' -> M
  _   -> error "rom: character not roman"

dec :: RomanChar -> Int
dec c = case c of
 I -> 1
 V -> 5
 X -> 10
 L -> 50
 C -> 100
 D -> 500
 M -> 1000
