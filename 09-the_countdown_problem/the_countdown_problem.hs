data Op = Add | Sub | Mul | Div

instance Show Op where
         show Add = "+"
         show Sub = "-"
         show Mul = "*"
         show Div = "/"

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
         show (Val n) = show n
         show (App o l r) = "(" ++ show l ++ show o ++ show r ++ ")"

apply :: Op -> Int -> Int -> Int
apply Add m n = m + n
apply Sub m n = m - n
apply Mul m n = m * n
apply Div m n = m `div` n

valid :: Op -> Int -> Int -> Bool
valid Add m n = m <= n
valid Sub m n = m > n
valid Mul m n = m <= n && m /= 1 && n /= 1
valid Div m n = m `mod` n == 0 && n /= 1

removeEl :: Int -> [a] -> [a]
removeEl i xs = take i xs ++ drop (i+1) xs

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = [ (xs !! i) : rest | i <- [0..length xs - 1], rest <- permutations (removeEl i xs)]

choices :: [a] -> [[a]]
choices xs = permutations xs ++ [ cs | i <- [0..length xs - 1], cs <- choices (removeEl i xs)]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

type Result = (Expr,Int)

combine :: Result -> Result -> [Result]
combine (l,m) (r,n) = [(App o l r, apply o m n) | o <- [Add,Sub,Mul,Div]
                                                , valid o m n]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n)]
results ns = [ res | (ls,rs) <- split ns
                   , l <- results ls
                   , r <- results rs
                   , res <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ e | cs <- choices ns
                     , (e,r) <- results cs
                     , r == n]
