data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata deriving (Eq, Show)
data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChances deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size deriving (Eq, Show)

myCar     = Car Mini (Price 140000)
urCar     = Car Mazda (Price 20000)
clownCar  = Car Tata (Price 900)
doge      = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _)  = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane (Car _ _)   = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu (Plane _ _) = Mini

------- Arity of data declarations -----

-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- proudct of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)


{- algebraic data types are called such becase we can describe
   the patterns of argument structures using two basic operations
   product and sum

   the cardinality of a datatype is the number of possible values
   it defines.  That number can be as small as 1 or infinite

   For example the cardinality of Bool is 2

   Int8 has valid values of whole numbers from -128 to 127,
   or a cardinality of 256

   the cardinality of

   1. data PugType = PugData is 1
   2. data Airline = PapuAir
                   | CatapultsR
                   | TakeYourChancesUnited is 3
   3 int16 is 65536

   unary constructors have the same cardinality
   as the types that inhabit them - basically
   the identity function

    -}

-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Liliac
--                 deriving Show
--
-- type Gardener = String
--
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

-- Normal Form?

data NormalGarden = Gardenia String
                  | Daisy String
                  | Rose String
                  | Liliac String
                  deriving Show


data GuessWhat =
  ChickenButt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b
                } deriving (Eq, Show)
--419

----- OOPs it got updated

data OperatingSystem = GnuPlusLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)
data ProgrammingLanguage = Haskell
                         | Clojure
                         | Agda
                         | Idris
                         | JavaScript
                         deriving (Eq, Show)
data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Clojure, Agda, Idris, JavaScript]

-- Generate all Programmers

allProgrammers :: [Programmer]
allProgrammers = [
  Programmer { os = y, lang = x } |
  x <- allLanguages,
  y <- allOperatingSystems
  ]


-- Who needs a building pattern?

data ThereYet =
  There Integer Float String Bool
  deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusss :: ThereYet
yusss = notQuite False

--- Deconstruction

newtype Name  = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
  Farmer Name Acres FarmerType deriving Show


isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

--- a -> b is Exponential? b^a

{-
  a -> b -> c
  (c ^ b) ^ a
  c ^ (a * b)
-}

data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

--and so on


-- Higher kinded data types
type DocVersion = String

data EsResultFound a =
  EsResultFound { _version :: DocVersion
                , _source  :: a
  } deriving (Eq, Show)


--
-- data Product a b =
--   a :&: b
--   deriving (Eq, Show)

--- Homemade list

data List a = Nil | Cons a (List a)



-- Binary Tree
data BinaryTree a =
   Leaf
 | Node (BinaryTree a) a (BinaryTree a)
 deriving (Eq, Ord, Show)




insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: BinaryTree a -> (a -> b) -> BinaryTree b
mapTree Leaf _ = Leaf
mapTree (Node left a right) f = Node (mapTree left f) (f a) (mapTree right f)


inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = (foldTree f (f a (foldTree f z right)) left)


testFoldTree :: IO ()
testFoldTree =
  if foldTree (+) 0 testTree == 6
  then putStrLn "Fold passes"
  else putStrLn "Fold fails"

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder Passes"
  else putStrLn "preorder Fails"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder passes"
  else putStrLn "Postorder fails"

testInOrder :: IO ()
testInOrder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder passes"
  else putStrLn "Inorder fails"

main :: IO ()
main = do
  testPreorder
  testPostorder
  testInOrder
  testFoldTree
