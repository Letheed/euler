module Pb80
  ( pb80
  ) where

import LibProblem
import Data.Char
import Data.List
import Data.Number.BigFloat

type BF = BigFloat (PrecPlus20 (PrecPlus20 (PrecPlus20 Prec50)))

uBound :: Int
uBound = 100

pb80 :: Computation
pb80 =  answer' . sum . concatMap (map digitToInt . take 100 . filter isDigit . show . sqrtPrec) $ xs
  where xs = [1..uBound] \\ map (^2) [1..sqrtInt uBound]
        sqrtPrec :: Int -> BF
        sqrtPrec = sqrt . fromIntegral
        sqrtInt  = floor . sqrt . fromIntegral
