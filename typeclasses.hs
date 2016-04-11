-- Chapter 6
-- Eq, Num, Ord, Enum, Show


-- Typeclasses and types are opposites
-- a declaration of a type defines how that type in particular
-- is created, a declaration of a typeclass defines how a set of types
-- are consumed.

-- typeclasses are a means of adhoc polymorphism

-- Any typeclass which implements EQ can be tested
-- for equality

-- :info Bool
-- instance Bounded Bool - Bounded for types that have an upper
-- and lower bound
-- instance Enum Bool - Enum for things that can be enumerated
-- instance Eq Bool - Eq for things that can be tested for equality
-- instance Ord Bool - Ord for things that can be put in a sequential
-- order
-- instance Read Bool - Read parses strings into things - do don't use
-- instance Show Bool - Show renders things into strings

-- Eq
-- == :: Eq a => a -> a -> Bool
-- /= :: Eq a => a -> a -> Bool

-- Num
-- + :: a -> a -> a .. and so on



-- Exercies

-- 1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- max is Ord a => a -> a -> a
-- length is Foldable t => t a -> Int
-- max 3 5
-- 5

-- 2. compare (3 * 4) (3 * 5)
-- (*) is Num a => a -> a -> a
-- Num is Ord
-- compare 12 15
-- compare is Ord a => a -> a Ordering
-- LT?

-- 3. compare "Julie" True
-- since compare is a -> a -> a Ordering,
-- arguments must be the same concrete type, won't work

-- 4. (5 + 3) > (3 + 6)
-- everything starts out as the more general type - Num
--  8 > 9
-- Num implements Ord
-- False


-- Enum
--- things that are enumerable, which have known predeccesors
--- and successors.

--- succ 5
--- 6

-- Show (and IO)
-- :t print
-- print :: Show a => a -> IO ()

-- Typeclasses are dispatched by type.  defined by a set of operations
-- and values all instances will provide.  instances are unique
-- pairings of the typeclass
class BasicEq a where
  isEqual :: a -> a -> Bool

class Numberish a where
  fromNumber    :: Integer -> a
  toNumber      :: a -> Integer
  defaultNumber :: a

data Age = Age Integer deriving (Eq, Show)
data Year = Year Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1998

instance BasicEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _     _     = False

data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = True


stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _  _ = False


data Trivial = Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True

-- Something a little less trivial
data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

data Date =
  Date DayOfWeek Int

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _   Fri = LT
  compare _   _   = EQ

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) _   _   = True

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

-- Exercises
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i)   (TisAnInt i')   = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _               _              = False

-- Ord instances

add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a  -> a
addWeird x y =
  if x > 1
    then x + y
    else x

-- Final exercises
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

----

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

--
i :: Num a => a
i = 1

f :: Fractional a => a
f = 1/2

myX = a :: Int
sigmund' :: Int -> Int
sigmund' x = myX
