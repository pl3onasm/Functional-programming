-----------------------------------------------------------
-- Exercise 4.4

-- defines the or operator, treating each case separately
or0 :: Bool -> Bool -> Bool
True `or0` True = True
True `or0` False = True
False `or0` True = True
False `or0` False = False

-- more concise version, using the fact that `or` only
-- returns False if both inputs are False
or1 :: Bool -> Bool -> Bool
False `or1` False = False
_ `or1` _ = True

-- we can also inverse the order of the equations of or',
-- so that the case where True is returned is the first one
or2 :: Bool -> Bool -> Bool
True `or2` _ = True
False `or2` b = b

-- another way to define or, is to use guards
or3 :: Bool -> Bool -> Bool
or3 a b
  | a == b = a
  | otherwise = True

-- yet another way to define or, is to use De Morgan's law
or4 :: Bool -> Bool -> Bool
or4 a b = not (not a && not b)

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-4.hs
  ghci> (4 == 3) `or0` (4 /= 3)
  True
  ghci> (2 > 3) `or4` (5 < 3)
  False

-}