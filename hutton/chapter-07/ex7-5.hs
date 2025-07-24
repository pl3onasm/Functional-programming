import Prelude hiding (curry, uncurry)

-----------------------------------------------------------
-- Exercise 7.5

-- converts a function that takes a pair of arguments into
-- a function that takes two arguments, one at a time, i.e.
-- a function of type (a, b) -> c into a function of type
-- a -> b -> c, so that it becomes curried and can be
-- partially applied
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

-- converts a function that takes two arguments, one at a
-- time, i.e. a function of type a -> b -> c into a 
-- function that takes a pair of arguments, i.e. a function 
-- of type (a, b) -> c, so that it becomes uncurried and 
-- can be applied to a pair of arguments at once
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

-----------------------------------------------------------

{- 
  To show that curry and uncurry are inverses of each 
  other, we can show that for any suitable function f:

    uncurry (curry f) = f
  and
    curry (uncurry f) = f

  This means converting back and forth between curried and
  uncurried forms preserves the original function.

  Proof:
  1. For uncurry (curry f):
        uncurry (curry f) (x, y)
        = (curry f) x y
        = f (x, y)

  2. For curry (uncurry f):
        curry (uncurry f) x y
        = uncurry f (x, y)
        = f (x, y)

-}