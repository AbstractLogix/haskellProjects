import Data.List
import System.IO

primeNumbers = [3,5,7,11]

morePrimes  = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 : []

mulList = [[3,5,7],[11,13,17]]

morePrimes2 = 2 : morePrimes

pow3List = [3^n | n <- [1..10]]

multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith

bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addressses = ["123 Main", "234 North", "567 South"]

namesNAddresses = zip names addressses

addMe :: Int -> Int -> Int
--funcName param1 param2 = operations (returned value)
addMe x y = x + y

sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can Drive"
whatAge 18 = "you can do stuff"
whatAge _ = "Nothing important"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0


whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "kindergarten"
    | (age > 6) && (age <= 10) = "Elementary"
    | otherwise = "Go to college"



batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "terrible batting average"
    | avg <= 0.250 = "average player"
    | avg <= 0.280 = "you're doing pretty good"
    | otherwise = "you're a superstar"
      where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "your list is empty"
getListItems (x:[]) = "your list starts with " ++ show x
getListItems (x:y:[]) = "your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first item is " ++ show x ++ " the rest of the items are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

-- Higher Order Functions --

times4 :: Int -> Int
times4 x = x * 4

listTimes4 n = map times4 [1..n]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4

threePlusList = map adds3 [1,2]

dbl1To10 = map (\x -> x * 2) [1..10]

doubleEvenNumber y =
  if (y `mod` 2 /= 0)
     then y
     else y * 2

getClass :: Int -> String
getClass n = case n of
   5 -> "Go to Kindergarten"
   6 -> "Go to elementary"
   _ -> "go away"

-- Enumerated types--
data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                deriving Show


barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOf = print(barryBonds Outfield)

-- Custom types --
data Customer = Customer String String Double
    deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

getName :: Customer -> String
getName (Customer n _ _) = n


data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Rock Scissors = "Rock beats Scissors"
shoot Scissors Paper = "Scissors beat Paper"
shoot Scissors Rock = "Scissors loses to Rock"
shoot Paper Scissors = "Paper loses to Scissors"
shoot Rock Paper = "Rock loses to Paper"
shoot _ _ = "Error"

-- defining two different versions of a type --

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
-- $ is basically a () replacement What's on the right takes president--
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 -y)

-- . is used to chain funcs:  output on the right passed to the input on the left --

sumValue = putStrLn (show ( 1 + 2))

sumValue2 = putStrLn . show $ 1 + 2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100


-- Type Classes --
data Employee = Employee { name :: String,
                           position :: String,
                           idNum :: Int
                           } deriving (Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 3333}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001}

isSamPam = samSmith == pamMarx

samSmithData = show samSmith

data ShirtSize = S | M | L

instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvail = S `elem` [S, M, L]

theSize = show S


-- custome type class --
class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newSize = areEqual M M


-- input and output --
sayHello = do
  putStrLn "what's your name"
  name <- getLine
  putStrLn $ "hello" ++ name

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("random line of text")
  hPutStrLn theFile ("second line of rand text")
  hClose theFile

readFromFile = do
  theFile2 <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile2
  putStr contents
  hClose theFile2

----------------------

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]
fibs n = fib !! n


 {-  how to create modules

module SampFunctions (getClass, doubleEvenNumbers) where
  getClass
  doubleEvenNumbers

-}



{-
main = do
  putStrLn "what's your name?"
  name <- getLine
  putStrLn ("Hello " ++ name)
-}
