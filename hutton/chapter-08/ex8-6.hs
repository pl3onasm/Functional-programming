-----------------------------------------------------------
-- Exercise 8.6

data Expr = Val Int | Add Expr Expr
  deriving (Show, Eq)

-- replaces each Val constructor by the function f 
-- and each Add constructor by the function g
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2) 

-- evaluates an expression to an integer value
eval :: Expr -> Int
eval = folde id (+)

-- computes the number of values in an expression
size :: Expr -> Int
size = folde (const 1) (+)

-----------------------------------------------------------

{- Examples:

ghci> :l ex8-6
ghci> exp = Add (Val 1) (Add (Val 2) (Val 3))
ghci> eval exp
6
ghci> size exp
3
ghci> exp = Add (Val 3) (Add (Val 6) (Add (Val 5) (Val 2)))
ghci> eval exp
16
ghci> size exp
4

-}