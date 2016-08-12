module Pb96
  ( pb96
  ) where

import LibIO
import LibProblem

import Data.Char
import Data.List
import Data.List.Batteries
import Data.Maybe
import Data.Ord
import qualified Data.Matrix as M

data Elem = Fixed Int
          | List [Int]

isFixed :: Elem -> Bool
isFixed e = case e of
  Fixed _ -> True
  _       -> False

isList :: Elem -> Bool
isList e = case e of
  List _ -> True
  _      -> False

newtype Grid = Grid (M.Matrix Elem)

idGrid :: Grid -> Int
idGrid (Grid grid) = digit 1 * 100 + digit 2 * 10 + digit 3
  where digit col = fromIntegral $
          case M.getElem 1 col grid of
            Fixed n -> n
            List  _ -> error "idGrid: unsolved element"

initialize :: Grid -> Grid
initialize grid@(Grid g0) = foldl' propagateFix grid fixedElems
  where fixedElems = [(row, col, n) | row <- [1..9], col <- [1..9]
                                    , let e = M.getElem row col g0, isFixed e
                                    , let Fixed n = e]

isGrid :: [String] -> Bool
isGrid gridLines = length gridLines == 9 && all isGridLine gridLines

isGridLine :: String -> Bool
isGridLine line = length line == 9 && all isDigit line

parseFile :: String -> [Grid]
parseFile file = mapMaybe parseGrid grids
  where grids = parse . lines $ file
          where parse fileLines
                  | null fileLines = []
                  | otherwise      = grid : parse rest
                  where (grid, rest) = span isGridLine . dropWhile (not . isGridLine) $ fileLines

parseGrid :: [String] -> Maybe Grid
parseGrid gridLines
  | isGrid gridLines = Just grid
  | otherwise        = Nothing
  where grid = initialize . Grid . M.fromLists . map (map (mkElem . digitToInt)) $ gridLines
          where mkElem n = case n of
                  0 -> List [1..9]
                  _ -> Fixed (fromIntegral n)

propagateFix :: Grid -> (Int, Int, Int) -> Grid
propagateFix (Grid grid) (row, col, n) = Grid (foldl' updateGrid grid updatedElems)
  where updateGrid g (i, j, e) = M.setElem e (i, j) g
        updatedElems = upedRow ++ upedCol ++ upedSquare
          where upedRow = [(row, j, uped e) | j <- [1..9], let e = M.getElem row j grid, isList e]
                upedCol = [(i, col, uped e) | i <- [1..9], let e = M.getElem i col grid, isList e]
                upedSquare = [(i, j, uped e) | i <- is, j <- js, let e = M.getElem i j grid, isList e]
                  where is = filter (row /=) [i0..i0+2] where i0 = 1 + 3 * ((row-1) `quot` 3)
                        js = filter (col /=) [j0..j0+2] where j0 = 1 + 3 * ((col-1) `quot` 3)
                uped e = case e of
                  List  l -> List (delete n l)
                  Fixed _ -> error "uped: element fixed"

solve :: Grid -> Maybe Grid
solve grid = case next of
  NoSolution       -> Nothing
  Guess (i, j) lst -> listToMaybe . mapMaybe solve $ [fix n (i, j) grid | n <- lst]
  Elem  (i, j) n   -> solve . fix n (i, j) $ grid
  Filled           -> Just grid
  where next = nextStep grid
        fix n (i, j) (Grid g) = propagateFix grid' (i, j, n)
          where grid' = Grid $ M.setElem (Fixed n) (i, j) g

data Next = NoSolution
          | Guess (Int, Int) [Int]
          | Elem  (Int, Int) Int
          | Filled

nextStep :: Grid -> Next
nextStep (Grid grid)
  | null nexts = if null lists then Filled else Guess (row, col) lst
  | otherwise  = head nexts
  where lists = [(i, j, l) | i <- [1..9], j <- [1..9], let e = M.getElem i j grid, isList e, let List l = e]
        nexts = [ if null lst then NoSolution else Elem (i, j) (head lst)
                | (i, j, lst) <- lists, alone lst]
        (row, col, lst) = minimumBy (comparing (length . thd)) lists
          where thd (_, _, c) = c

pb96 :: Computation
pb96 = do
  grids <- parseFile <$> readProblem 96 "sudokus"
  let solutions = mapMaybe solve grids
  let msg =  "solved: " ++ show (length solutions) ++ "/" ++ show (length grids) ++ " – "
  msgAnswer $ (msg ++) . show . sum . map idGrid $ solutions
