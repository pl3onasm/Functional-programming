import Data.Foldable
import Data.Traversable

-----------------------------------------------------------
-- Exercise 14.3

data Mmaybe a = Mnothing 
              | Mjust a
              deriving Show 

instance Foldable Mmaybe where

  -- fold :: Monoid a => Mmaybe a -> a
  fold Mnothing  = mempty
  fold (Mjust x) = x

  -- foldMap :: Monoid b => (a -> b) -> Mmaybe a -> b
  foldMap _ Mnothing  = mempty  
  foldMap f (Mjust x) = f x

  -- foldr :: (a -> b -> b) -> b -> Mmaybe a -> b
  foldr _ z Mnothing  = z
  foldr f z (Mjust x) = f x z

  -- foldl :: (a -> b -> a) -> a -> Mmaybe b -> a
  foldl _ z Mnothing  = z
  foldl f z (Mjust x) = f z x

instance Traversable Mmaybe where

  -- traverse :: Applicative f => 
  --             (a -> f b) -> Mmaybe a -> f (Mmaybe b)
  traverse _ Mnothing  = pure Mnothing
  traverse f (Mjust x) = Mjust <$> f x

instance Functor Mmaybe where

  -- fmap :: (a -> b) -> Mmaybe a -> Mmaybe b
  fmap _ Mnothing  = Mnothing
  fmap f (Mjust x) = Mjust (f x)

-----------------------------------------------------------

{- 
  Note that we had to use a new data type Mmaybe to avoid
  conflicts with the existing implementation. This is also
  why we defined a Functor instance for Mmaybe, which is
  required for the Traversable instance.

  The Foldable instance for the Maybe type allows us to  
  reduce a Maybe value to a single summary value using
  various folding operations. If the Maybe is Nothing,
  we return a neutral result (such as mempty or the base
  case z). If it is Just x, we apply the given function
  to x.

  - fold extracts the value or returns mempty
  - foldMap applies a function to the value and returns 
    the result (of monoid type). If the value is absent, 
    it returns the monoid identity (mempty)
  - foldr and foldl apply a binary function to the value
    and an initial accumulator (z), returning z if empty

  The Traversable instance allows us to apply an effectful
  function to the value inside a Just, returning a Maybe 
  wrapped in the same Applicative context. If the Maybe is 
  Nothing, we return a pure Nothing — that is, a Nothing 
  inside the applicative.

  Examples:
    ghci> :l ex14-3.hs
    ghci> foldMap show (Mjust 42)
    "42"
    ghci> foldr (-) 10 (Mjust 5)
    5
    ghci> foldl (*) 4 (Mjust 3)
    12
    ghci> traverse (\x -> Just (x * 2)) (Mjust 10)
    Just (Mjust 20)
    ghci> traverse (\x -> [x , x + 1]) (Mjust 5)
    [Mjust 5, Mjust 6]

-}