-----------------------------------------------------------
-- Exercise 12.6

instance Monad ((->) a) where
  
  -- generally: return :: b -> m b
  -- substituting m with ((->) a) gives us:
  -- return :: b -> (a -> b)
  return x = \_ -> x

  -- generally: (>>=) :: m b -> (b -> m c) -> m c
  -- substituting m with ((->) a) gives us:
  -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  f >>= g = \x -> let b = f x
                      h = g b
                  in h x

-----------------------------------------------------------

{- 
  These definitions can be written more concisely:

    return = const
    f >>= g = \x -> g (f x) x

  The first defines return as a function that lifts a value
  into a constant function, one that ignores its input and
  always returns the same result.

  The second defines the bind operator (>>=) as an operator
  that sequences two computations that depend on the same
  input x of type a.
  
  The first function f extracts an intermediate result of 
  type b from the input x. The second function g uses that 
  result to produce a new computation of type a -> c, which
  is then applied to the same input x to yield the final 
  result.
  
-}