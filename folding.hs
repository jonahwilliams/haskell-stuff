-- Folds ---
--- catamorphisms because
--- "down" or against, a means of deconstructing data

-- foldr
-- Foldable t => (a -> b -> b) -> b -> t a -> b

mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = x + mySum xs

myLength :: [a] -> Integer
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myProduct :: [Integer] -> Integer
myProduct []     = 0
myProduct (x:xs) = x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs
------


---Fold Right
foldr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 f acc []      = acc
foldr1 f acc (x:xs)  = f x (foldr1 f acc xs)

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f acc xs =
  case xs of
    []      -> acc
    (x:xs)  -> f x (foldr2 f acc xs)
