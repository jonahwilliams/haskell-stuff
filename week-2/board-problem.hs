type Board = [[Char]]
type Coord = (Int, Int)

data Node = Node { position :: Coord
                 , visited  :: [Coord]
                 , match    :: [Char]
                 } deriving Show

-- Calculates valid neighbor positions given n x m dimensions of board
--  and i, j current position
neighbors :: Int -> Int -> Coord -> [(Coord)]
neighbors n m (i, j) = [(x, y) | x <- [(i - 1) .. (i + 1)],
                            y <- [(j - 1) .. (j + 1)],
                            x > -1,
                            y > -1,
                            x < n,
                            y < m ]

-- Every position which has the correct starting character is turned into a
---  Node to start the recursive process.  we check every character in the board
initial :: Board -> [Char] -> [Node]
initial b []     = []
initial b (c:cs) =
  let n = (length b)
      m = (length (b !! 0))
      start = [(i, j) | i <- [0..(n - 1)],
                        j <- [0..(m - 1)],
                        ((b !! i) !! j) == c ]
  in map (\c -> Node { position = c, visited = [c], match = cs }) start

-- given a board and a list of partialy matched nodes, find possible next nodes
-- return true if we completely match a word
findWord :: Board -> [Node]  -> Bool
findWord _ [] = False
findWord _ (Node { position = (i, j), visited = vs, match = []    }):xs = True
findWord b (Node { position = (i, j), visited = vs, match = (c:cs)}):xs =
  let n = (length b)
      m = (length (b !! 0))
      predicate = \(x,y) -> and [(notElem (x,y) vs), (((b !! x) !! y) == c)]
      raw = filter predicate (neighbors n m (i, j))
      ns = map (\t -> Node { position = t, visited = t : vs, match = cs }) raw
  in findSolution b (xs ++ ns)


testBoard :: Board
testBoard = [
  ['a', 'c', 'l', 'b'],
  ['r', 'e', 'b', 'u'],
  ['l', 'n', 'c', 's'],
  ['c', 'm', 'n', 'o']]

theDoot :: Board -> [Char] -> Bool
theDoot _ [] = True
theDoot b xs =
  let starting = initial b xs
  in findWord b starting
