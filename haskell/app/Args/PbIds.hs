{-# LANGUAGE MultiWayIf, ViewPatterns, TypeApplications #-}

module Args.PbIds
  ( isPbIdS
  , parsePbIdS
  , warnNotPbIdS
  ) where

import Problems

import Batteries
import Control.Applicative
import Control.Monad
import Data.Char

-- | Problem ID or list of problem ID predicate for `String`.
isPbIdS :: String -> Bool
isPbIdS = isIntOrEnumS

-- | Integer or integer enumeration predicate for `String`.
isIntOrEnumS :: String -> Bool
isIntOrEnumS s = isIntS s || isEnumS s

-- | Integer predicate for `String`.
isIntS :: String -> Bool
isIntS [] = False
isIntS s  = all isDigit s

-- | Integer enumeration predicate for `String`.
isEnumS :: String -> Bool
isEnumS s = isEnumFromTo s || isEnumFromThenTo s
  where someDigits (span isDigit -> ([], _)) = Nothing
        someDigits (span isDigit -> (_,  r)) = Just r
        isEnumFromTo (someDigits -> Just ('.':'.': (someDigits -> Just []))) = True
        isEnumFromTo  _                                                      = False
        isEnumFromThenTo (someDigits -> Just (',': stt)) = isEnumFromTo stt
        isEnumFromThenTo  _                              = False


-- | Parse a `String` as a problem ID or a enumeration of problem IDs.
parsePbIdS :: String -> Maybe [PbId]
parsePbIdS = parseIntOrEnumS >=> traverse mkPbId

-- | Parse a `String` as a integer or an enumeration of integers.
parseIntOrEnumS :: String -> Maybe [Integer]
parseIntOrEnumS s = (singleton <$> parseIntS s) <|> parseEnumS s

-- | Parse a `String` as an integer.
parseIntS :: String -> Maybe Integer
parseIntS s = if isIntS s then Just (read s) else Nothing

-- | Parse a `String` as an enumeration of integers.
parseEnumS :: String -> Maybe [Integer]
parseEnumS s = mEnumFromTo s <|> mEnumFromThenTo s
  where someDigits (span isDigit -> ([], _)) = Nothing
        someDigits (span isDigit -> t)       = Just t
        mEnumFromTo     = fmap (\(n1, n2) -> [read n1..read n2]) . parseEnumFromTo
        mEnumFromThenTo = fmap (\(n1, n2, n3) -> [read n1, read n2..read n3]) . parseEnumFromThenTo
        parseEnumFromTo (someDigits -> Just (n1, '.':'.': (someDigits -> Just (n2, [])))) = Just (n1, n2)
        parseEnumFromTo  _                                                                = Nothing
        parseEnumFromThenTo (someDigits -> Just (n1, ',': (parseEnumFromTo -> Just (n2, n3)))) = Just (n1, n2, n3)
        parseEnumFromThenTo  _                                                                 = Nothing

-- | Print a warning for a list of arguments
-- that do not parse as problem IDs.
warnNotPbIdS :: [String] -> IO ()
warnNotPbIdS []    = pure ()
warnNotPbIdS [arg] = putWarnS $ show arg ++ " not a proper number or enum"
warnNotPbIdS args  = putWarnS $ show args ++ " not proper numbers or enums"
