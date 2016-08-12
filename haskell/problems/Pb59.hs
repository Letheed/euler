module Pb59
  ( pb59
  ) where

import LibIO
import LibProblem

import Batteries
import Data.Bits
import Data.Char
import qualified Data.Text    as T
import qualified Data.Text.IO as T

type Key = String
type Dict = [T.Text]
type Cipher = String

pb59 :: Computation
pb59 = do
  cipher <- parseCipher <$> readProblemT 59 "cipher"
  dict <- T.lines <$> T.readFile "/usr/share/dict/words"
  let (key, msg) = decrypt cipher dict
  let sumMsg     = ordS msg
  answer sumMsg $ "key: \"" ++ key ++ "\", message is an extract from a bad fantasy book"

parseCipher :: T.Text -> Cipher
parseCipher = map (chr . readIntT) . splitListT . T.stripEnd

keys :: [String]
keys = [[c1, c2, c3] | c1 <- alphabet, c2 <- alphabet, c3 <- alphabet]
  where alphabet = ['a'..'z']

decrypt :: Cipher -> Dict -> (Key, String)
decrypt cipher dict = head decryptTries
  where decryptTries = [(key, msg) | key <- keys, let msg = xorCycleS cipher key, isEnglish dict msg]

xorCycleS :: String -> Key -> String
xorCycleS str = map chr . zipWith xor (map ord str) . cycle . map ord

isEnglishChar :: Char -> Bool
isEnglishChar c = 'a' <= c && c <= 'z'
               || 'A' <= c && c <= 'Z'
               || c == '-' || c == '\''

isEnglish :: Dict -> String -> Bool
isEnglish dict msg = hasSpaceEarly && looksEnglish && inDict firstWords
  where hasSpaceEarly = ' ' `elem` take 10 msg
        nWords        = 6
        looksEnglish  = lengthIs nWords firstWords
        firstWords    = filter (T.all isEnglishChar) . take nWords $ msgWords
        inDict        = all (\w -> w `elem` dict || T.toLower w `elem` dict)
        msgWords      = filter (not . T.null) (trim . T.pack <$> words msg)
          where trim = T.dropAround (not . isLetter)
