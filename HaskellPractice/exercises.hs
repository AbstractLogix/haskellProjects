import Data.Maybe
import Data.Char
import Data.List

charChecker :: Char -> Bool
charChecker 'a' = True
charChecker _ = False

stringChecker :: String -> Bool
stringChecker "hello" = True
stringChecker _ = False

trim :: String -> String
trim (x:xs)
  | x == ' ' = xs
  | otherwise =  (x:xs)

twice :: (a -> a) -> a -> a
twice f x = f ( f x )

quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c
  = let d = sqrt (b^2 - 4*a*c)
        x1 = (-b + d) / (2 * a)
        x2 = (-b - d) / (2 * a)
      in (x1, x2)

convert :: [Int] -> [Bool]
convert [] = []
convert (x:xs)
  | x == 1 = True : convert xs
  | x == 0 = False : convert xs
  | otherwise = convert xs

member0 :: String -> Bool
member0 xs = elem '0' xs

-- Exercise 9
addJust :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJust [] [] = []
addJust xs [] = xs
addJust [] ys = ys
--addJust (x:xs) (y:ys) = stripJust(x) + stripJust(y) : addJust xs ys

--addJust (x:xs) (y:ys) | x && y == Just x && Just y = justWrap $ stripJust(x) + stripJust(y) : addJust xs ys

--addJust xs ys = (foldr (+) 0 $ catMaybes xs) + (foldr (+) 0 $ catMaybes ys)



{-
 make a function to spec that works with Inetgers only....
 then worry about the Maybe type.
-}


intTransformer :: Int -> [Int]
intTransformer n = n : []

sortCheck :: [Int] -> Bool
sortCheck [] = False
sortCheck xs = if
                 (sort $ map intToDigit xs) == (map intToDigit xs)
                 then True
                 else False

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
