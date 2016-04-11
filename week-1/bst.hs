-- Binary Tree (Unbalanced)
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f Nil                 = Nil
  fmap f (Node a left right) =
    Node (f a) (fmap f left) (fmap f right)

singleton :: a -> Tree a
singleton x = Node x Nil Nil

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Nil = singleton x
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

contains :: (Ord a) => a -> Tree a -> Bool
contains x Nil = False
contains x (Node a left right)
    | x == a = True
    | x < a  = contains x left
    | x > a  = contains x right

depth :: Tree a -> Int
depth Nil = 0
depth (Node a left right) =
  1 + (max (depth left) (depth right))
