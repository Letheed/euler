module Pb102
  ( pb102
  ) where

import LibIO
import LibProblem

import qualified Data.Text as T

data Vector = Vector Double Double

data Triangle = Triangle Vector Vector Vector

pb102 :: Computation
pb102 = do
  triangles <- map parseTriangle . T.lines <$> readProblemT 102 "triangles"
  answer' . length . filter containsOrigin $ triangles

parseTriangle :: T.Text -> Triangle
parseTriangle str = Triangle (Vector x1 y1) (Vector x2 y2) (Vector x3 y3)
  where [x1, y1, x2, y2, x3, y3] = map (fromIntegral . readIntT) . splitListT $ str

containsOrigin :: Triangle -> Bool
containsOrigin (Triangle a b c) = u >= 0 && v >= 0 && u + v <= 1
  where u = (dot11 * dot02 - dot01 * dot12) * invDenom
        v = (dot00 * dot12 - dot01 * dot02) * invDenom
        invDenom = recip (dot00 * dot11 - dot01 * dot01)
        dot00 = v0 `vdot` v0
        dot01 = v0 `vdot` v1
        dot02 = v0 `vdot` v2
        dot11 = v1 `vdot` v1
        dot12 = v1 `vdot` v2
        v0 = c `vsub` a
        v1 = b `vsub` a
        v2 = vneg a

vsub (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)
vdot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2
vneg (Vector x y) = Vector (-x) (-y)
