-----------------------------------------------------------
-- Exercise 16.11

{- 
  Given are the following data types:

    data Expr = Val Int | Add Expr Expr 

    type Stack = [Int]

    type Code = [Op]

    data Op = PUSH Int | ADD 

  Also given are the following function definitions:

    eval :: Expr -> Int
    eval (Val n) = n
    eval (Add x y) = eval x + eval y

    comp :: Expr -> Code
    comp (Val n) = [PUSH n]
    comp (Add x y) = comp x ++ comp y ++ [ADD]

  Our task is to construct a more efficient compile 
  function comp' that rewrites comp in accumulator style,
  avoiding the expensive use of list append (++). This 
  means we want to rewrite comp in terms of a function
  comp' that takes an expression and an accumulator, which 
  is empty at the start and built up as we traverse the 
  expression tree.

    comp :: Expr -> Code
    comp e = comp' e []

  To achieve this, we perform constructive induction on the
  structure of the expression e in the following property:

    p(e): comp' e c = comp e ++ c

  Intuitively, comp' takes an expression e and an 
  accumulator c, and composes the code for e with the code
  in c.

  Base case: p(Val n)

      comp' (Val n) c
    =   {specification of comp'}
      comp (Val n) ++ c
    =   {definition of comp}
      [PUSH n] ++ c
    =   {list concatenation}
      PUSH n : c

  Inductive case: p(Add x y)

      Induction hypothesis:
        p(x): comp' x c = comp x ++ c
        p(y): comp' y c = comp y ++ c

      comp' (Add x y) c
    =   {specification of comp'}
      comp (Add x y) ++ c
    =   {definition of comp}
      comp x ++ comp y ++ [ADD] ++ c
    =   {associativity of ++}
      comp x ++ comp y ++ ([ADD] ++ c)
    =   {applying ++}
      comp x ++ comp y ++ (ADD : c)
    =   {induction hypothesis p(y)}
      comp x ++ comp' y (ADD : c)
    =   {induction hypothesis p(x)}
      comp' x (comp' y (ADD : c))

  Thus, the property p holds for all expressions e, and 
  we define the function comp' as follows:

    comp' :: Expr -> Code -> Code
    comp' (Val n) c     = PUSH n : c
    comp' (Add x y) c   = comp' x (comp' y (ADD : c))

  □

  -- Example:
  -- comp (Add (Val 2) (Val 3)) 
  --   = comp' (Add (Val 2) (Val 3)) []
  --   = comp' (Val 2) (comp' (Val 3) (ADD : []))
  --   = comp' (Val 2) (PUSH 3 : ADD : [])
  --   = PUSH 2 : (PUSH 3 : ADD : [])
  --   = [PUSH 2, PUSH 3, ADD]

-}