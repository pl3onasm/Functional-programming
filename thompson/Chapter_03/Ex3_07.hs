import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 3.7

-- | defines logical nAnd (Exercise 3.5)
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

-- | alternative definition for nAnd
nAnd' :: Bool -> Bool -> Bool
nAnd' True True = False
nAnd' _    _    = True

-- | defines exclusive or (Exercise 3.1)
exclOr :: Bool -> Bool -> Bool
exclOr x y = (x && not y) || (y && not x)

-- | book's definition of exclusive or
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

-- | property: nAnd and nAnd' are equivalent
prop_nAnds :: Bool -> Bool -> Bool
prop_nAnds x y = nAnd x y == nAnd' x y

-- | property: exclOr and exOr are equivalent
prop_exclOrs :: Bool -> Bool -> Bool
prop_exclOrs x y = exclOr x y == exOr x y

-----------------------------------------------------------

{- 
  Testing in GHCi 

  ghci> :load Ex3-07
  ghci> quickCheck prop_nAnds
  +++ OK, passed 100 tests.
  ghci> quickCheck prop_exclOrs
  +++ OK, passed 100 tests.

-}