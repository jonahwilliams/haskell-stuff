{-
Given a matrix of characters like
    [['a', 'c', 'l', 'b'],
     ['r', 'e', 'b', 'u'],
     ['l', 'n', 'c', 's'],
     ['c', 'm', 'n', 'o']]

determine if a given string like "care" can
be found by navigating through the matrix without
repeating characters - ie snake rules
-}

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

type Coord = (Int, Int)
type Board = Map.Map Char [Coord]


buildMap :: [[Char]] -> Board
buildMap cs =
   Map.fromListWith (++)
    $ concat
    $ map (\(ds, i) -> map (\(c, j) -> (c, [(i, j)]) ) $ zip ds [1..])
    $ zip cs [1..]


isNeighbor :: Coord -> [Coord] -> Bool
isNeighbor (i,j) [] = True
isNeighbor (i,j) ((x,y):xs) =
  abs (i - x) < 2 && abs (j - y) < 2


testBoard :: [[Char]]
testBoard = [['a', 'c', 'l', 'b'],
             ['r', 'e', 'b', 'u'],
             ['l', 'n', 'c', 's'],
             ['c', 'm', 'n', 'o']]


findMatch :: Board -> [Coord] -> [Char] -> Bool
findMatch _ xs []     = True
findMatch b xs (y:ys) =
  or $ map (\z -> findMatch b (z:xs) ys)
    $ filter (\z -> (isNeighbor z xs) && notElem z xs)
    $ Maybe.fromMaybe []
    $ Map.lookup y b
