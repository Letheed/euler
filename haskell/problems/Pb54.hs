module Pb54
  ( pb54
  ) where

import LibIO
import LibProblem

import Batteries
import Data.Char
import Data.Function
import Data.Ix
import Data.List
import Data.Maybe

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq, Ord, Show)

data Card = Card
  { cardFace  :: Int
  , cardSuit  :: Suit
  } deriving (Eq, Ord, Show)

parseCard :: String -> Card
parseCard [face, suit] = Card face' suit'
  where face'
          | isDigit face && inRange (2,9) d = d
          | face == 'T' = 10
          | face == 'J' = 11
          | face == 'Q' = 12
          | face == 'K' = 13
          | face == 'A' = 14
          | otherwise   = error "parseCard: unknown face value"
          where d = digitToInt face
        suit' = case suit of
          'C' -> Clubs
          'D' -> Diamonds
          'H' -> Hearts
          'S' -> Spades
          _   -> error "parseCard: can't parse suit"
parseCard _ = error "parseCard: wrong number of characters for a card"

faceEq :: Card -> Card -> Bool
faceEq = (==) `on` cardFace

data Player = Player
  { playerCards  :: [Card]
  } deriving (Show)

parsePlayer :: [Card] -> Player
parsePlayer cards@[_, _, _, _, _] = Player (sort cards)
parsePlayer _ = error "parsePlayer: wrong number of cards for a player"

data Game = Game Player Player

parseGame :: String -> Game
parseGame s = Game player1 player2
  where (cards1, cards2) = splitAt 5 . map parseCard . words $ s
        player1 = parsePlayer cards1
        player2 = parsePlayer cards2

data Hand = HighCard
          | Pair          Int
          | TwoPair       Int Int
          | Three         Int
          | Straight      Int
          | Flush
          | FullHouse     Int Int
          | Four          Int
          | StraightFlush Int
          deriving (Eq, Ord, Show)

highCard :: a -> Maybe Hand
highCard = const (Just HighCard)

tryN :: Int -> Player -> Maybe Hand
tryN n (Player cards)
  | n `elem` [2, 3, 4] = triedN
  | otherwise          = Nothing
  where triedN = case filter (lengthIs n) . groupBy faceEq $ cards of
          (c:_):_ -> Just (handCons (cardFace c))
          _       -> Nothing
        handCons = case n of
          2 -> Pair
          3 -> Three
          4 -> Four
          _ -> error "handCons: no such constructor"

tryTwoPair :: Player -> Maybe Hand
tryTwoPair (Player cards) = case filter (lengthIs 2) . groupBy faceEq $ cards of
  [c:_, c':_] -> Just (TwoPair (cardFace c) (cardFace c'))
  _           -> Nothing

tryStraight :: Player -> Maybe Hand
tryStraight (Player cards@(Card face _:_))
  | isStraight = Just (Straight faceMax)
  | otherwise  = Nothing
  where isStraight = cardFaces == [face..face+4] || cardFaces == [2, 3, 4, 5, ace] where ace = 14
        cardFaces  = map cardFace cards
        faceMax    = last cardFaces
tryStraight _ = error "tryStraight: player has no cards"

tryFlush :: Player -> Maybe Hand
tryFlush (Player (Card _ suit:cards))
  | isFlush   = Just Flush
  | otherwise = Nothing
  where isFlush = all ((suit ==) . cardSuit) cards
tryFlush _ = error "tryFlush: player has no cards"

tryFullHouse :: Player -> Maybe Hand
tryFullHouse (Player cards) = case groupBy faceEq cards of
 [[c, _, _], [c', _]] -> Just (FullHouse (cardFace c) (cardFace c'))
 [[c, _], [c', _, _]] -> Just (FullHouse (cardFace c') (cardFace c))
 _                    -> Nothing

tryStraightFlush :: Player -> Maybe Hand
tryStraightFlush player = case (tryFlush player, tryStraight player) of
  (Just _, Just (Straight face)) -> Just (StraightFlush face)
  _                              -> Nothing

parseHand :: Player -> Hand
parseHand player = head . mapMaybe ($ player) $ [ tryStraightFlush, tryN 4, tryFullHouse, tryFlush
                                                , tryStraight, tryN 3, tryTwoPair, tryN 2, highCard
                                                ]

data Winner = Player1
            | Player2
            deriving (Eq, Show)

winner :: Game -> Winner
winner (Game player1 player2)
  | hand1  > hand2  = Player1
  | hand2  > hand1  = Player2
  | cards1 > cards2 = Player1
  | cards2 > cards1 = Player2
  | otherwise       = error "it's a tie"
  where hand1  = parseHand player1
        hand2  = parseHand player2
        cards1 = reverse (playerCards player1)
        cards2 = reverse (playerCards player2)

pb54 :: Computation
pb54 = do
  games <- map parseGame . lines <$> readProblem 54 "poker"
  let n = length . filter (Player1 ==) . map winner $ games
  answer n $ show n ++ " games won by Player 1"
