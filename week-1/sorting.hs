
-- InsertionSort
--- Because why not?

insert :: (Ord a) => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) =
  if x < y
    then (x:y:ys)
    else [y] ++ (insert x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertionSort' xs []

insertionSort' :: (Ord a) => [a] -> [a] -> [a]
insertionSort' [] ys     = ys
insertionSort' (x:xs) ys = insertionSort' xs (insert x ys)



-- Quicksort
--- take an item from the head, recursively call quicksort,
--- partitioning the list in two based on being bigger than x.
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted


-- Mergesort
--- Recursive divide each list in half, then merge back together
--- In a sorted order.
merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) []     = (x:xs)
merge [] (y:ys)     = (y:ys)
merge (x:xs) (y:ys) =
  if x < y
    then [x] ++ (merge xs (y:ys))
    else [y] ++ (merge (x:xs) ys)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort []     = []
mergeSort (x:[]) = [x]
mergeSort xs =
  let n  = quot (length xs) 2
      ll = take n xs
      uu = drop n xs
  in merge (mergeSort ll) (mergeSort uu)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Largest number under 100,000 that is divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible =
  let p = (\x -> x `mod` 3829 == 0)
  in  head (filter p [100000,99999..1])

-- Sum of all odd squares that are less than 10000
soqlt :: (Integral a) => a
soqlt = sum (takeWhile (\x -> x < 10000) (filter odd (fmap (^2) [1..])))

-- Coltaz Seqs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\x -> length x > 15) (map chain [1..100]))
