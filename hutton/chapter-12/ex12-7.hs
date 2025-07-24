-----------------------------------------------------------
-- Exercise 12.7

data Expr a = Var a 
            | Val Int 
            | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val n) = Val (n)
  fmap g (Add e1 e2) = Add (fmap g e1) (fmap g e2)

instance Applicative Expr where

  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> x = fmap f x
  (Val n) <*> _ = Val n
  (Add e1 e2) <*> x = Add (e1 <*> x) (e2 <*> x)

instance Monad Expr where

  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var x) >>= f = f x
  (Val n) >>= _ = Val n
  (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)

-----------------------------------------------------------

{-
  The Functor instance defines how to map a function
  over the structure of Expr:
    - Var applies the function to its value
    - Val remains unchanged (since it's a constant)
    - Add applies the function recursively to both
      subexpressions.

  The Applicative instance enables applying functions
  embedded in Expr to other Expr values:
    - pure wraps a value in the simplest Expr context (Var)
    - (<*>) handles function application while preserving 
      the structure of Expr

  The Monad instance allows sequencing computations in the 
  Expr context:
    - (>>=) extracts values from Var and applies the given 
      function
    - Val ignores the function
    - Add applies the bind recursively to both
      subexpressions
-}