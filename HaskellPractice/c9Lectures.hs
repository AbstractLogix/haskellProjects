double x = x + x

quadruple :: Num a => a -> a

quadruple = double . double

f x y = (x-y)*(x-y)

squareDifference x xs = map (f x) xs

ab3 n | n >= 0 = n
      | otherwise = -n

