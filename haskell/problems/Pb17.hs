module Pb17
  ( pb17
  ) where

import LibProblem

ones = [ "one"
       , "two"
       , "three"
       , "four"
       , "five"
       , "six"
       , "seven"
       , "eight"
       , "nine"
       ]

teens = [ "ten"
        , "eleven"
        , "twelve"
        , "thirteen"
        , "fourteen"
        , "fifteen"
        , "sixteen"
        , "seventeen"
        , "eighteen"
        , "nineteen"
        ]

decades = [ "twenty"
          , "thirty"
          , "forty"
          , "fifty"
          , "sixty"
          , "seventy"
          , "eighty"
          , "ninety"
          ]

pb17 :: Computation
pb17 = answer totalLen $ show totalLen ++ " letters needed to write numbers from 1 to 10³"

onesLen :: Int
onesLen = sum . map length $ ones

hundredNums :: Int
hundredNums = onesLen + teensLen + twentyAndAbove
  where teensLen       = sum . map length $ teens
        twentyAndAbove = sum . map ((onesLen +) . (10 *) . length) $ decades

totalLen :: Int
totalLen = hundredsLen + andLen + 10 * hundredNums + length ("one" ++ "thousand")
  where hundredsLen = 100 * (onesLen + 9 * length "hundred")
        andLen      =  99 * 9 * length "and"
