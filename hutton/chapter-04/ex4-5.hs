-----------------------------------------------------------
-- Exercise 4.5

-- defines logical && using conditional expressions and
-- following the pattern matching definition:
--    True && True = True
--    _ && _ = False
land :: Bool -> Bool -> Bool
land a b = if a then
                  if b then True
                  else False
                else False

-- or using the case expression:
land' :: Bool -> Bool -> Bool
land' a b = case a of
                True -> case b of
                          True -> True
                          False -> False
                False -> False

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-5.hs
  ghci> (5 < 6) `land` (7 < 8)  
  True
  ghci> (5 < 6) `land` (7 > 8)
  False

-}