-----------------------------------------------------------
-- Exercise 3.9

-- | checks if three integers are all different
threeDifferent :: Integer -> Integer -> Integer -> Bool 
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)

-----------------------------------------------------------

{-

ghci> :load Ex3-09
ghci> threeDifferent 3 4 3
False

Why do we get this answer? Let us calculate it step 
by step:

  threeDifferent 3 4 3
=   { applying threeDifferent }
  (3 /= 4) && (4 /= 3) && (3 /= 3)
=   { evaluating the comparisons }
  True && True && False
=   { evaluating the first && }
  True && False
=   { evaluating the remaining && }
  False 

-}