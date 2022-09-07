-- are all logical values in a list true
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | otherwise  = and' xs

and'' :: [Bool] -> Bool
and'' [] = True
and'' (x:xs) = (x /= False) && and'' xs

and''' :: [Bool] -> Bool
and''' [] = True
and''' (x:xs) = x && and''' xs

-- concatenate a list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- make a list of n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- get the nth element of a list
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

-- is a value an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | x == n = True
               | otherwise = elem' n xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' _ [] = False
elem'' n (x:xs) = (x == n) || elem'' n xs

-- merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- merge sort
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take l xs)) (msort (drop l xs))
           where l = div (length xs) 2
