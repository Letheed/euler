module Pb55
  ( pb55
  ) where

import LibProblem

pb55 :: Computation
pb55 = answer n $ show n ++ " Lychrel numbers < 10⁴"
  where n = length . filter isLychrel $ [1..9999]

isLychrel :: Integer -> Bool
isLychrel = not . any isPalindromic . tail . take 50 . iterate reverseAdd
  where reverseAdd n = n + mirrorNum n
