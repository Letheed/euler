{-# LANGUAGE OverloadedStrings #-}

module Pb107
  ( pb107
  ) where

import LibIO
import LibProblem
import Data.List
import Data.Ord
import qualified Data.Text as T

data Edge = Edge Int Int Int
          deriving (Show)

edgeWeight :: Edge -> Int
edgeWeight (Edge _ _ w) = w

data UGraph = UGraph Int [Int] [Edge]
            deriving (Show)

weight :: UGraph -> Int
weight (UGraph _ _ edges) = sum . map edgeWeight $ edges

pb107 :: Computation
pb107 = do
  graph <- parseGraph <$> readProblemT 107 "network"
  let mst = minimumSpanningTree graph
  answer' $ weight graph - weight mst

minimumSpanningTree :: UGraph -> UGraph
minimumSpanningTree (UGraph sizeTot nodes@(n0:_) edges) = go 1 [n0] []
  where go size ns es
          | size == sizeTot = UGraph sizeTot nodes es
          | otherwise       = go (size+1) (n:ns) (e:es)
          where n = if i `elem` ns then j else i
                e@(Edge i j _) = head . filter isAdjacent $ edges
                isAdjacent (Edge i j _) = (i `elem` ns && j `notElem` ns) || (i `notElem` ns && j `elem` ns)

parseGraph :: T.Text -> UGraph
parseGraph graphStr = UGraph size [1..size] edges
  where size = length nodesStr
        nodesStr = T.lines graphStr
        edges = sortBy (comparing edgeWeight) . concatMap parseEdges . zip [2..] . drop 1 $ nodesStr
          where parseEdges (i, line) = map (uncurry (Edge i) . fmap readIntT) edgesStr
                  where edgesStr = filter (("-" /=) . snd) . take i . zip [1..] . splitListT $ line
