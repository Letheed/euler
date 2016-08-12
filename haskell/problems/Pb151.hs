module Pb151
  ( pb151
  ) where

import LibProblem(msgAnswer, answer', Computation)

pb151 :: Computation
pb151 = msgAnswer $ show $ fun (1,1,1,1)


fun (0,0,0,1) = 0
fun (0,0,1,0) = fun (0,0,0,1) + 1
fun (0,1,0,0) = fun (0,0,1,1) + 1
fun (1,0,0,0) = fun (0,1,1,1) + 1
fun (a,b,c,d) = (pickA + pickB + pickC + pickD) / (a + b + c + d)
  where pickA | a > 0     = a * fun (a-1,b+1,c+1,d+1)
              | otherwise = 0
        pickB | b > 0     = b * fun (a,b-1,c+1,d+1)
              | otherwise = 0
        pickC | c > 0     = c * fun (a,b,c-1,d+1)
              | otherwise = 0
        pickD | d > 0     = d * fun (a,b,c,d-1)
              | otherwise = 0
