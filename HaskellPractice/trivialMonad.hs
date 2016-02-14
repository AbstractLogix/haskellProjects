data W a = W a deriving Show

return1 :: a -> W a
return1 x = W x

fmap1 :: (a -> b) -> (W a -> W b)
fmap1 f (W x) = W (f x)


