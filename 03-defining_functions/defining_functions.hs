-- conditional expression
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

-- guarded equations
safetail'' :: [a] -> [a]
safetail'' xs | null xs = []
              | otherwise = tail xs

-- pattern matching
safetail''' :: [a] -> [a]
safetail''' [] = []
safetail''' (x:xs) = xs

-- valid definitions of logical or using pattern matching
or' :: Bool -> Bool -> Bool
or' True True = True
or' True False = True
or' False True = True
or' False False = False

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _ = True

or''' :: Bool -> Bool -> Bool
or''' False b = b
or''' True _ = True

or'''' :: Bool -> Bool -> Bool
or'''' False b = b
or'''' _ _ = True

(|||) :: Bool -> Bool -> Bool
False ||| b = b
True ||| _ = True

-- valid definitions of logical and using conditional expressions
and' :: Bool -> Bool -> Bool
and' b0 b1 = if (b0 == True) && (b1 == True) then True else False

and'' :: Bool -> Bool -> Bool
and'' b0 b1 = if (b0 == True) then
                 if (b1 == True) then True else False
                 else False

and''' :: Bool -> Bool -> Bool
and''' b0 b1 = if b0 == True then b1 else False
