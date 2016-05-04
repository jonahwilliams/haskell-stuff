-- Lists

data List a = Cons a (List a) | Nil deriving (Show)

addList :: a -> List a -> List a
addList x xs = Cons x (xs)


concatList :: List a -> List a -> List a
concatList Nil ys = ys
concatList xs Nil = xs
concatList (Cons x xs) ys = Cons x (concatList xs ys)

update :: List a -> Int -> a -> List a
update (Cons x xs) 0 y = Cons y xs
update (Cons x xs) i y = Cons x (update xs (i - 1) y)
update Nil i y = Nil


-- Ex 2.1 write a function of type list a -> list list a that takes a list xs
-- and returns a list of all of the suffiexes of xs in decreasing order of length

suffiexes :: List a -> List (List a)
suffiexes Nil = Nil
suffiexes (Cons x xs) = Cons (Cons x xs) (suffiexes xs)


-- 2.2 Binary Search Trees

data Tree a = Nil | Branch a (Tree a) (Tree a)

empty :: Tree a
empty = Nil

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Branch x Nil Nil
insert (Branch a l r) x
    | x == a = Branch a l r
    | x > a  = Branch a l (insert r x)
    | x < a  = Branch a (insert l x) r

member :: (Ord a) => Tree a -> a -> Boolean
member Nil _ = False
member (Branch a l r) x
    | x == a = True
    | x > a  = member r x
    | x < a  = member l x

-- 2.3 The above search makes approx 2d worst case searches, rewrite to have
-- worst case d + 1

member2 :: (Ord a) => Tree a -> a -> Boolean
member2 Nil _ = False
member2 tree value =
  member2' tree tree value

member2' :: (Ord a) => Tree a -> Tree a -> a -> Boolean
