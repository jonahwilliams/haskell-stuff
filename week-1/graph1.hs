--- graph stuff
--- Directed Graph

type Vertex = Int
type Cost = Int
type Edge = (Vertex, Vertex)
type Graph = [Edge]



-- Are two nodes connected?
-- Only works with undirected graphs because we don't account for
-- already visited nodes
connected1 :: Vertex -> Vertex -> Graph -> Bool
connected1 start stop graph =
  let connected = [e | e <- graph, fst e == start]
      done = elem stop (fmap snd connected)
  in done || (any (\e -> connected1 (snd e) stop graph) connected)


connected :: Vertex -> Vertex -> Graph -> Bool
connected start stop graph =
  connected2 start stop [] graph

connected2 :: Vertex -> Vertex -> [Vertex] -> Graph -> Bool
connected2 start stop visited graph =
  let connected = [e | e <- graph, fst e == start, notElem (snd e) visited]
      done = elem stop (fmap snd connected)
  in done || (any (\e -> connected2 (snd e) stop (start : visited) graph) connected)

makeUndirected :: Graph -> Graph
makeUndirected graph =
  graph ++ (fmap (\a -> (snd a, fst a)) graph)

example :: Graph
example = [ (1, 2)
          , (2, 3)
          , (3, 6)
          , (3, 5)
          , (3, 7)
          , (7, 9)
          ]
