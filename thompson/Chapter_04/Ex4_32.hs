module Chapter_04.Ex4_32 where

-----------------------------------------------------------
-- Exercise 4.32

-- | Computes 2^n using logarithmic exponentiation
power2 :: Integer -> Integer
power2 exp
  | exp == 0  = 1
  | even exp  = sqr (power2 (exp `div` 2))
  | otherwise = 2 * power2 (exp - 1)
  where sqr n = n * n

-----------------------------------------------------------