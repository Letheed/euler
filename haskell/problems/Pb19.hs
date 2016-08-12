module Pb19
  ( pb19
  ) where

import LibProblem

data Month = M28 | M29 | M30 | M31

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
             deriving(Enum, Eq)

pb19 :: Computation
pb19 = answer n $ show n ++ " 1st of the month were sundays in the 20th century"
  where n = length . filter isSun1st . zip weekdays . concatMap intoDays . concatMap intoMonths $ years
        years = [1901..2000] :: [Int]
        weekdays = drop 1 $ cycle [Mon .. Sun] -- 1st Jan. 1901 was a Tuesday.
        isSun1st (wd, d) = d == 1 && wd == Sun
        intoMonths y = [M31, feb, M31, M30, M31, M30, M31, M31, M30, M31, M30, M31]
          where feb = if y `rem` 4 == 0 && y `rem` 400 /= 0 then M29 else M28
        intoDays m = case m of
                       M31 -> [1..31] :: [Int]
                       M30 -> [1..30]
                       M29 -> [1..29]
                       M28 -> [1..28]
