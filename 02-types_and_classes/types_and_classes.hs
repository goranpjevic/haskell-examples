-- curried functions
add' :: Int -> (Int -> Int)
add' x y = x+y

add'' :: Int -> Int -> Int
add'' x y = x+y

add''' :: Int -> Int -> Int
add''' x = (x+)

-- polymorphic function
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- overloaded function
addone :: Num a => a -> a
addone x = x+1
