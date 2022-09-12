-- natural numbers
data Nat = Succ Nat | Zero deriving Show

-- add two natural numbers
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- multiply two natural numbers
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

-- expression
data Expr = Val Nat
         | Add Expr Expr
         | Mult Expr Expr deriving Show

eval :: Expr -> Nat
eval (Val n) = n
eval (Add m n) = add (eval m) (eval n)
eval (Mult m n) = mult (eval m) (eval n)

folde :: (Nat -> Nat) -> (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> Expr -> Nat
folde vf af mf = f
              where
                f (Val n) = vf n
                f (Add m n) = af (f m) (f n)
                f (Mult m n) = mf (f m) (f n)

eval' :: Expr -> Nat
eval' = folde id add mult

-- binary tree
data Tree a = Node (Tree a) (Tree a)
            | Leaf a deriving Show
