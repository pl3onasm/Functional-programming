-- The compiler in the book for arithmetic expressions

type Stack = [Int]

data Expr = Val Int 
          | Add Expr Expr
          deriving Show

data Code = HALT 
          | PUSH Int Code 
          | ADD Code
          deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)



{-
  Example usage:

  ghci> let expr = Add (Val 1) (Add (Val 2) (Val 3))
  ghci> eval expr
  6
  ghci> let code = comp expr
  ghci> code
  PUSH 1 (PUSH 2 (PUSH 3 (ADD (ADD HALT))))
  ghci> head (exec code [])
  6

-}