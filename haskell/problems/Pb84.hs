{-# LANGUAGE MultiWayIf, NamedFieldPuns, ScopedTypeVariables #-}

module Pb84
  ( pb84
  ) where

import LibProblem (msgAnswer, answer', Computation)
import Data.List
import Data.Ord
import System.Random
import qualified Data.Vector.Unboxed as UV

default (Int)

data Game = Game { position    :: Int
                 , throws      :: [Int]
                 , doubles     :: Int
                 , comState    :: Int
                 , chanceState :: Int
                 , stat        :: UV.Vector Int
                 }

(go, jail, go2jail) = (0, 10, 30)
communityChests  = [2, 17, 33]
chances          = [7, 22, 36]
destChances      = [go, 5, jail, 11, 24, 39]
railwayCompanies = [5, 15, 25, 35]
utililyCompanies = [12, 28]

nTurns :: Int
nTurns = 100000

nDiceSides :: Int
nDiceSides = 4

pb84 :: Computation
pb84 = do
  g <- getStdGen
  let game = initGame g
  let Game { stat } = iterate play game !! nTurns
  let prob :: UV.Vector Double = UV.map (\cnt -> fromIntegral cnt / fromIntegral nTurns) stat
  let mostPopular :: [(Int, Double)] = take 3 . sortBy (flip $ comparing snd) . zip [0..] . UV.toList $ prob
  --mapM_ (printf "%02d" . fst) mostPopular
  msgAnswer $ (show mostPopular)

initGame :: (RandomGen g) => g -> Game
initGame g = Game 0 throws 0 0 0 stat
  where stat   = UV.generate 40 (\pos -> if pos == 0 then 1 else 0)
        throws = randomRs (1, nDiceSides) g

updateStat :: UV.Vector Int -> Int -> UV.Vector Int
updateStat stat position = stat UV.// [(position, stat UV.! position + 1)]

moveTo :: Int -> Game -> Game
moveTo position game@Game { stat } = game { position, stat = updateStat stat position }

updateGameStat :: Game -> Game
updateGameStat game@Game { position, stat } = game { stat = updateStat stat position }

play :: Game -> Game
play game@Game { position, throws, doubles }
  | doubles' == 3 || pos == go2jail = moveTo jail gameThrew
  | pos `elem` communityChests      = drawCommunityChest gameMoved
  | pos `elem` chances              = drawChance gameMoved
  | otherwise                       = moveTo pos gameThrew
  where d1:d2:throws' = throws
        doubles'      = if d1 == d2 then doubles + 1 else 0
        pos           = (position + d1 + d2) `rem` 40
        gameThrew     = game { throws = throws', doubles = doubles' `rem` 3 }
        gameMoved     = gameThrew { position = pos }

drawCommunityChest :: Game -> Game
drawCommunityChest game@Game { comState }
  | comState == 0 = moveTo go   gameDrew
  | comState == 1 = moveTo jail gameDrew
  | otherwise     = updateGameStat gameDrew
  where gameDrew = game { comState = (comState + 1) `rem` 16 }

drawChance :: Game -> Game
drawChance game@Game { position, chanceState }
  | chanceState < 6           = moveTo (destChances !! chanceState) gameDrew
  | chanceState `elem` [6, 7] = moveTo nextRailwayCompany           gameDrew
  | chanceState == 8          = moveTo nextUtilityCompany           gameDrew
  | chanceState == 9          = if | pos `elem` communityChests -> drawCommunityChest gameMoved
                                   | otherwise                  -> moveTo pos gameDrew
  | otherwise                 = updateGameStat gameDrew
  where gameDrew           = game { chanceState = (chanceState + 1) `rem` 16 }
        nextRailwayCompany = nextCompany position railwayCompanies
        nextUtilityCompany = nextCompany position utililyCompanies
        pos                = position - 3
        gameMoved          = gameDrew { position = pos }

nextCompany :: Int -> [Int] -> Int
nextCompany position companies
  | null next = head companies
  | otherwise = head next
  where next = dropWhile (position >=) companies
