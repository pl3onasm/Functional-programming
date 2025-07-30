import Data.Foldable
import Data.Traversable

-----------------------------------------------------------
-- Exercise 14.3

data Maybe' a = Nothing' 
              | Just' a
              deriving Show 

instance Foldable Maybe' where

  -- fold :: Monoid a => Maybe' a -> a
  fold Nothing'  = mempty
  fold (Just' x) = x

  -- foldMap :: Monoid b => (a -> b) -> Maybe' a -> b
  foldMap _ Nothing'  = mempty  
  foldMap f (Just' x) = f x

  -- foldr :: (a -> b -> b) -> b -> Maybe' a -> b
  foldr _ z Nothing'  = z
  foldr f z (Just' x) = f x z

  -- foldl :: (a -> b -> a) -> a -> Maybe' b -> a
  foldl _ z Nothing'  = z
  foldl f z (Just' x) = f z x

instance Traversable Maybe' where

  -- traverse :: Applicative f => 
  --             (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing'  = pure Nothing'
  traverse f (Just' x) = Just' <$> f x

instance Functor Maybe' where

  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)

-----------------------------------------------------------

{- 
  Note that we had to use a new data type Maybe' to avoid
  conflicts with the existing implementation for Maybe.
  We also defined a Functor instance for Maybe', which is
  required for the Traversable instance.

  The Foldable instance for the Maybe type allows us to  
  reduce a Maybe value to a single summary value using
  various folding operations. If the Maybe is Nothing,
  we return a neutral result (such as mempty or the base
  case z). If it is Just x, we apply the given function
  to x.

  - fold extracts the value from Just or returns mempty
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
    ghci> foldMap show (Just' 42)
    "42"
    ghci> foldr (-) 10 (Just' 5)
    5
    ghci> foldl (*) 4 (Just' 3)
    12
    ghci> traverse (\x -> Just (x * 2)) (Just' 10)
    Just (Just' 20)
    ghci> traverse (\x -> [x , x + 1]) (Just' 5)
    [Just' 5, Just' 6]

-}