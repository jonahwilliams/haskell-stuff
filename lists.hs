-- Chapter 9 Lists
--- this thing, and then some more stuff

-- In haskell, lists sever as both finite and infinite lists
-- due to lazyness

-- data [] a = [] | a : [a]

-- 9.3

myHead :: [a] -> a
myHead (x : _) = x

myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail _        = []

-- these patterns are non exhaustive

-- MAYBE

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

myEnumFromTo :: (Ord a, Enum a) => a -> a -> [a]
myEnumFromTo start stop =
  if (start == stop)
  then start : []
  else start : myEnumFromTo (succ start) stop

myTake :: Int -> [a] -> [a]
myTake 0 _        = []
myTake n (x : xs) = x : myTake (n - 1) xs
myTake n []       = []

myDrop :: Int -> [a] -> [a]
myDrop 0 l        = l
myDrop n (x : xs) = myDrop (n - 1) xs
myDrop n []       = []

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n l = ((take n l), (drop n l))

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x : xs) =
  if (p x) then
    x : myTakeWhile p xs
  else
    []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x : xs) =
  if (p x) then
    myDropWhile p xs
  else
    x : xs

-- goddamn, better clean this up
makeSentence :: String -> [String]
makeSentence []    = []
makeSentence words =
  let pred = (\c -> c /= ' ')
      cont = dropWhile pred words
  in [takeWhile pred words] ++ if (length cont > 1)
    then
      (makeSentence (tail cont))
    else
      []

-- Generators and List Comprhensions
example = [ x^2 | x <- [1..10]]

-- stuff
mySqr = [x^2 | x <- [1..5]]
-- 1 4 9 16 25
ex1 = [x | x <- mySqr, rem x 2 == 0]
-- 4 16

ex2 = [(x, y) | x <- mySqr, y <- mySqr, x < 5, y > 50]
-- []


-- Standard functions
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny p (x:xs) = (p x) || myAny p xs
