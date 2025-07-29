import Data.Monoid

-----------------------------------------------------------
-- Exercise 14.2

newtype Func a b = F (a -> b)

instance (Monoid b) => Semigroup (Func a b) where
  -- (<>) :: Func a b -> Func a b -> Func a b
  F f <> F g = F (\x -> f x <> g x)

instance (Monoid b) => Monoid (Func a b) where
  -- mempty :: Func a b
  mempty = F (\_ -> mempty)

  -- mappend defaults to (<>)
  mappend = (<>)

-----------------------------------------------------------

{- 
  This instance defines a Monoid for functions of type 
  (a -> b), provided that the result type b is itself a 
  Monoid. Since GHC8.4 and later, the Monoid class
  requires a Semigroup instance, so we define the
  Semigroup instance first. We also need to use a newtype
  wrapper Func to avoid conflicts with the existing 
  implementation in the Prelude.

  The idea is to combine two functions f and g:
  given an input x, the result of (f <> g) x is simply 
  f x <> g x, using the Monoid operation for type b.

  The identity element is the constant function that
  returns mempty for every input:
    mempty = F (\_ -> mempty)

  This satisfies the Monoid laws because:

  1. Identity:
     For any F f :: Func a b and any x :: a,
       (F f <> mempty) x = f x <> mempty = f x
       (mempty <> F f) x = mempty <> f x = f x

  2. Associativity:
         ((F f <> F g) <> F h) x 
       = (f x <> g x) <> h x
       = f x <> (g x <> h x)
       = (F f <> (F g <> F h)) x

  3. Closure:
     Since f x and g x are both of type b, and b is a 
     Monoid, their combination f x <> g x is again of 
     type b.

  Examples:
    ghci> :l ex14-2.hs
    ghci> let f = F (\x -> Sum (x * 2))
    ghci> let g = F (\x -> Sum (x - 3))
    ghci> let F h = f <> g

    ghci> h 5
    Sum {getSum = 12}   -- 2*5 + (5-3) = 10 + 2 = 12

    ghci> let F z = mempty :: Func Int (Sum Int)
    ghci> z 42
    Sum {getSum = 0}    -- the identity element for Sum

    ghci> let F z = mempty :: Func Int String
    ghci> z 42
    ""                  -- the identity element for String
-}
