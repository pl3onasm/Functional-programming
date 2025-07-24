-----------------------------------------------------------
-- Exercise 12.4

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z ([g x | (g, x) <- zip gs xs])

-----------------------------------------------------------

{- 
  This shows that a parameterized type such as list can be
  made into an applicative functor in more than one way. 
  If multiple instances are possible for a type, a wrapper 
  type such as ZipList is required to distinguish them.

  The standard Applicative instance for lists performs a 
  Cartesian product: it applies every function to every 
  value, producing all possible combinations.

  The ZipList instance, by contrast, applies functions and
  values element-wise — zipping the two lists together and
  stopping at the shorter list.

  For example, given two lists:
  - Functions: [(*2), (+3)]
  - Values: [1, 2, 3]
  The standard list applicative would produce:
      [(*2), (+3)] <*> [1, 2, 3]
    = [(*2) 1, (*2) 2, (*2) 3, (+3) 1, (+3) 2, (+3) 3]
    = [2, 4, 6, 4, 5, 6]
  The ZipList applicative would produce:
      Z [(*2), (+3)] <*> Z [1, 2, 3]
    = Z [(*2) 1, (+3) 2]
    = Z [2, 5]

  The Applicative instance for ZipList satisfies the
  Applicative laws:

  1. Preservation of identity:
     pure id <*> x = x

    Let x = Z xs
    
    LHS:
    We have pure id = Z (repeat id)
    So LHS becomes:
    Z (repeat id) <*> Z xs
      = Z ([id x | (id, x) <- zip (repeat id) xs])
      = Z xs

    RHS:
    x = Z xs

    So both sides yield the same ZipList.

  2. Preservation of application:
     pure (g x) = pure g <*> pure x

    Let g be a function and x be a value.

    LHS:
    pure (g x) = Z (repeat (g x))
    
    RHS:
    We have pure g = Z (repeat g) 
            pure x = Z (repeat x)
    So RHS becomes:
    Z (repeat g) <*> Z (repeat x)
      = Z ([g x | (g, x) <- zip (repeat g) (repeat x)])
      = Z (repeat (g x))

    So both sides yield the same ZipList.

  3. Interaction of pure and <*>:
     x <*> pure y = pure (\g -> g y) <*> x

    Let x = Z fs and y be a value.

    LHS:
    Z fs <*> pure y
      = Z fs <*> Z (repeat y)
      = Z [f y | (f, y) <- zip fs (repeat y)]
      = Z [f y | f <- fs]
    
    RHS:
    We have pure (\g -> g y) = Z (repeat (\g -> g y))
    So:
    Z (repeat (\g -> g y)) <*> Z fs
      = Z [(\g -> g y) f 
           | (\g -> g y, f) <- zip (repeat (\g -> g y)) fs]
      = Z [f y | f <- fs]

    So both sides yield the same ZipList.

  4. Associativity of <*>:
     x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

    Let x = Z fs, y = Z gs, z = Z xs

    LHS:
    x <*> (y <*> z)
      = Z fs <*> Z [g x | (g, x) <- zip gs xs]
      = Z [f (g x) | (f, (g, x)) <- zip fs (zip gs xs)]
      = Z [f (g x) | (f, g, x) <- zip3 fs gs xs]

    RHS:
    (pure (.) <*> x <*> y) <*> z
      = Z (repeat (.)) <*> Z fs
      = Z [(.) f | f <- fs]
      = Z [f . g | (f, g) <- zip fs gs]
      = Z [(f . g) x 
            | (f . g, x) <- zip (zipWith (.) fs gs) xs]
      = Z [f (g x) | (f, g, x) <- zip3 fs gs xs]

    So both sides yield the same ZipList.

  Thus, the applicative instance for ZipList is valid.

-}