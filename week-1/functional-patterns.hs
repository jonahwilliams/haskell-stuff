-- More Functional Patterns

myNum :: Integer
myNum = 1

myVal f = myNum

trip = (\x -> x * 3) :: Integer -> Integer

addOne = \x -> x + 1

addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive :: Ord a => a -> a -> a
addFive = \x -> \y -> (if x > y then y else x)


mflip f x y = f y x


-- Pattern matching
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False


data WherePenguinsLive =
    Galapagos
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Show, Ord)
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neiter employee is the boss"
    LT -> reportBoss e' e

fibonacci :: Integral => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
