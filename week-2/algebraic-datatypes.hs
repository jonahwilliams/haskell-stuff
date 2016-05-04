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
