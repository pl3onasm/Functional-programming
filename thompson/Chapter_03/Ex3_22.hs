module Chapter_03.Ex3_22 where

-----------------------------------------------------------
-- Exercise 3.22

-- | Returns the number of roots of a quadratic equation
--   with given coefficients a, b and c where a /= 0
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
  | dis < 0   = 0
  | dis == 0  = 1
  | otherwise = 2
    where dis = b^2 - 4*a*c


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex3_22
ghci> numberNDroots 1 1 1
0
ghci> numberNDroots 1 4 1
2
ghci> numberNDroots 1 2 1
1

-}