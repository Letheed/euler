module Pb67
  ( pb67
  ) where

import LibIO
import LibProblem

import qualified Data.Text as T

pb67 :: Computation
pb67 = do
  triangle <- map parseLine . T.lines <$> readProblemT 67 "triangle"
  let s = maxTrianglePathSum triangle
  answer s $ show s ++ " maximum path sum"
  where parseLine = map readIntT . T.split (== ' ')
