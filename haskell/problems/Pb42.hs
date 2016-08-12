module Pb42
  ( pb42
  ) where

import LibIO
import LibProblem

import Prelude hiding (words)
import Batteries
import qualified Data.Text as T

pb42 :: Computation
pb42 = do
  words <- splitQuotedListT <$> readProblemT 42 "words"
  let n = length . filter isTriangleWord $ words
  answer n $ show n ++ " triangular words"

isTriangleWord :: T.Text -> Bool
isTriangleWord word = alphaOrdT word `elemOrd` polygonals 3
