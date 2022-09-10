-- [f x | x <- xs, p x]
ff :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ff f p xs = map f (filter p xs)

-- redefinitions of map and filter using foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x y -> f x : y) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x y -> if p x then x : y else y) [] xs
