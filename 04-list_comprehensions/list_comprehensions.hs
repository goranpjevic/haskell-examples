-- generate prime numbers
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [p | p <- [1..n], prime p ]

-- generate pythagorean triples
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]

-- generate perfect numbers
perfect :: Int -> Bool
perfect n = sum (init (factors n)) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

perfects' :: Int -> [Int]
perfects' n = [x | x <- [1..n], sum (init (factors x)) == x]

-- scalar product
sp :: [Int] -> [Int] -> Int
sp xs ys = sum [x*y | (x,y) <- zip xs ys]

sp' :: [Int] -> [Int] -> Int
sp' xs ys = sum [xs!!i * ys!!i | i <- [0..length xs - 1]]

sp'' :: [Int] -> [Int] -> Int
sp'' xs ys = sum [xs!!i * ys!!i | i <- [0..n - 1]]
             where n = length xs
