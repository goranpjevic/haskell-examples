-- calculate the sum of a list of numbers
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' xs = foldr (+) 0 xs

sum''' :: Num a => [a] -> a
sum''' = foldr (+) 0

-- drop a given number of elements from the start of a list
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

-- remove the last element of a non-empty list
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

init'' :: [a] -> [a]
init'' (x:xs) | null xs = []
              | otherwise = x : init'' xs

init''' :: [a] -> [a]
init''' = reverse . tail . reverse

init'''' :: [a] -> [a]
init'''' xs = take (length xs - 1) xs
