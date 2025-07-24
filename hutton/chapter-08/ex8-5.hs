-----------------------------------------------------------
-- Exercise 8.5

data Expr = Val Int | Add Expr Expr
  deriving (Show, Eq)

-- replaces each Val constructor by the function f 
-- and each Add constructor by the function g
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)     = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2) 

-----------------------------------------------------------

{-
  An Expr is a binary tree structure where each node is
  either a value (Val n) or an addition (Add e1 e2).
  The function folde traverses this expression tree 
  recursively, applying a function f to each value
  node and a function g to each addition node.
  
  This is a generalization of folding over lists.  
  Just as foldr defines how to reduce a list,  
  folde defines how to reduce an expression tree  
  by specifying how to handle the two constructors:
  - Val, which is handled by the function f,
  - and Add, which is handled by the function g.

  It allows us to define many operations on expressions  
  by simply choosing different functions for f and g.

-}