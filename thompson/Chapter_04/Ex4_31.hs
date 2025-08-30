-----------------------------------------------------------
-- Exercise 4.31

-- | Computes the highest common factor of two integers
-- using Euclid's algorithm
hcf :: Integer -> Integer -> Integer
hcf a 0 = abs a
hcf a b = hcf b (a `mod` b)


-----------------------------------------------------------