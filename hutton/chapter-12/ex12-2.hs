-----------------------------------------------------------
-- Exercise 12.2

instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

-----------------------------------------------------------

{-
  The general type of fmap for a Functor is:
    fmap :: Functor f => (b -> c) -> f b -> f c

  Substituting ((->) a) for f, we get:
    fmap :: (b -> c) -> ((->) a b) -> ((->) a c)

  Since (a -> b) is syntactic sugar for ((->) a b),
  this simplifies to:
    fmap :: (b -> c) -> (a -> b) -> (a -> c)

  That is, if you have a function f :: a -> b, and 
  another function g :: b -> c, then you can map g over 
  the result of f, producing a new function h :: a -> c. 
  This is exactly what function composition does:
    fmap g f = g . f

  So fmap for functions is just (.), the composition 
  operator. It maps a function over the result of another
  function, effectively chaining them together.

  The Functor instance for ((->) a) also satisfies the 
  Functor laws:

  1. Identity:
    fmap id = id
    That is, id . f = f

  2. Composition:
    fmap (g . h) = fmap g . fmap h
    That is, (g . h) . f = g . (h . f)
    or equivalently: fmap (g . h) f = fmap g (fmap h f)

  These laws hold because:
  - Composing with the identity function does not change
    a function.
  - Function composition is associative, meaning that
    the grouping of function applications does not affect
    the result.

  Therefore, the Functor instance for ((->) a) is valid.
-}