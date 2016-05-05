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
    $ map (\(ds, i) -> map (\(c, j) -> (c, [(i, j)]) )
    $ zip ds [1..])
    $ zip cs [1..]


isNeighbor :: Coord -> Coord -> Bool
isNeighbor (i,j) (x,y) =
  abs (i - x) < 2 && abs (j - y) < 2


testBoard :: [[Char]]
testBoard = [['a', 'c', 'l', 'b'],
             ['r', 'e', 'b', 'u'],
             ['l', 'n', 'c', 's'],
             ['c', 'm', 'n', 'o']]

findMatch :: Board -> [Char] -> Bool
findMatch _ [] = True
findMatch b (y:ys) =
  or $ map (\z -> findMatch' b [z] ys)
    $ Maybe.fromMaybe []
    $ Map.lookup y b

findMatch' :: Board -> [Coord] -> [Char] -> Bool
findMatch' _ (x:xs) []     = True
findMatch' b (x:xs) (y:ys) =
  or $ map (\z -> findMatch' b (z:(x:xs)) ys)
    $ filter (\z -> isNeighbor x z && notElem z (x:xs))
    $ Maybe.fromMaybe []
    $ Map.lookup y b
