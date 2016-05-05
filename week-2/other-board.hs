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
type Board = Map.Map Coord Char

getNeighbors :: Coord -> [Coord]
getNeighbors (i,j) = [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]

buildMap :: [String] -> Board
buildMap cs =
   Map.fromList .
     concatMap (\(ds, i) -> fmap (\(c, j) -> ((i, j), c) ) $ zip ds [1..])
     $ zip cs [1..]

testBoard :: [String]
testBoard = [['a', 'c', 'l', 'b'],
             ['r', 'e', 'b', 'u'],
             ['l', 'n', 'c', 's'],
             ['c', 'm', 'n', 'o']]

findMatch :: [String] -> String -> Bool
findMatch b xs =
  any (\t -> findMatch' (Map.delete t b) t xs) . Map.keys $ buildMap b


findMatch' :: Board -> Coord -> String -> Bool
findMatch' _ _ []     = True
findMatch' b c (x:xs) =
  any (\t -> findMatch' (Map.delete t b) t xs) .
    filter (\t -> elem x $ Map.lookup t b)
    $ getNeighbors c
