-- sieve of eratosthenes
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [ x | x <- xs, mod x p /= 0]

-- generate prime numbers
primes :: [Int]
primes = sieve [2..]

-- check if a pair of primes are twins
twin :: (Int,Int) -> Bool
twin (x,y) = (x+2) == y

-- generate a list of twin primes
twinPrimes :: [(Int,Int)]
twinPrimes = filter twin (zip primes (tail primes))
