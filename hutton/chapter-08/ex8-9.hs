-----------------------------------------------------------
-- Exercise 8.9

-- In this exercise, we extend the abstract machine to
-- support the use of multiplication.
-- The abstract machine is a simple stack-based
-- interpreter for a small expression language

-- expressions can be either a value, an addition,
-- or a multiplication
data Expr = Val Int 
          | Add Expr Expr
          | Mult Expr Expr
  deriving (Show, Eq)

-- control stack for the abstract machine.
-- each operation is represented by an Op.
-- a continuation is a list of operations.
type Cont = [Op]

-- each operation is either:
-- - a pending evaluation of an expression (ADD or MULT),
-- - or an arithmetic operation with a known integer value
--   (ADDVAL or MULTVAL).
data Op = ADD Expr
        | MULT Expr
        | ADDVAL Int
        | MULTVAL Int
  deriving (Show, Eq)

-- evaluates an expression using the abstract machine
-- by recursively evaluating the sub-expressions
eval :: Expr -> Cont -> Int
eval (Val n) c    = exec c n
eval (Add x y) c  = eval x (ADD y : c)
eval (Mult x y) c = eval x (MULT y : c)

-- executes the control stack (continuation), applying
-- operations in the order they appear
exec :: Cont -> Int -> Int
exec [] n = n
exec (ADD y : c) n      = eval y (ADDVAL n : c)
exec (MULT y : c) n     = eval y (MULTVAL n : c)
exec (ADDVAL m : c) n   = exec c (n + m)
exec (MULTVAL m : c) n  = exec c (n * m)

-- evaluates a complete expression from scratch
-- by starting with an empty control stack
value :: Expr -> Int
value e = eval e []

-----------------------------------------------------------

{-
  Examples:

  ghci> :l ex8-9
  ghci> e1 = Add (Val 1) (Mult (Val 2) (Val 3))
  ghci> value e1
  7
  ghci> e2 = Mult (Add (Val 1) (Val 2)) (Val 3)
  ghci> value e2
  9
  ghci> e3 = Mult (Val 4) (Add (Val 5) (Val 6))
  ghci> value e3
  44
  ghci> value (Mult e1 e2)
  63
  ghci> value (Add e1 (Mult e2 e3))
  403
  ghci> value (Add e3 (Mult e2 e2))
  125

  A step-by-step evaluation of the expression e1
  would look like this:

    value (Add (Val 1) (Mult (Val 2) (Val 3)))
  =     { applying value }
  eval (Add (Val 1) (Mult (Val 2) (Val 3))) []
  =     { applying eval }
  eval (Val 1) [ADD (Mult (Val 2) (Val 3))]
  =     { applying eval }
  exec [ADD (Mult (Val 2) (Val 3))] 1
  =     { applying exec }
  eval (Mult (Val 2) (Val 3)) [ADDVAL 1]
  =     { applying eval }
  eval (Val 2) [MULT (Val 3), ADDVAL 1]
  =     { applying eval }
  exec [MULT (Val 3), ADDVAL 1] 2
  =     { applying exec }
  eval (Val 3) [MULTVAL 2, ADDVAL 1]
  =     { applying eval }
  exec [MULTVAL 2, ADDVAL 1] 3
  =     { applying exec }
  exec [ADDVAL 1] 6
  =     { applying exec }
  exec [] 7
  =     { applying exec }
  7

-}