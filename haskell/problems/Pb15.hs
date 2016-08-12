module Pb15
  ( pb15
  ) where

import LibProblem

gridSize :: Integer
gridSize = 20

pb15 :: Computation
pb15 = answer n $ show n ++ " such routes in a 20x20 grid"
  where n = (gridSize*2) `choose` gridSize
