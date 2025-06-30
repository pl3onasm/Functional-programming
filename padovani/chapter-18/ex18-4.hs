-----------------------------------------------------------
-- Exercise 18.4

-- generates the list of divisors of a number
divisors :: Integer -> [Integer]
divisors n = [ x | x <- [1..n], x `divides` n ]

divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

-- checks if a number is prime using trial division
-- by testing divisibility from 2 up to the square 
-- root of the number 
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = isp 3
  where
    isp k
      | k * k > n = True
      | n `mod` k == 0 = False
      | otherwise = isp (k + 2)

-- generates the list of prime divisors of a number
-- by filtering its divisors for primality
primeDivs :: Integer -> [Integer]
primeDivs n = filter isPrime (divisors n)

-- computes the prime factorization of a number
-- as a list of pairs, where each pair contains a prime
-- factor and its exponent in the factorization
factorize :: Integer -> [(Integer, Integer)]
factorize n = factor n (primeDivs n)
  where
    factor _ [] = []
    factor m (p : ps)
      | m `mod` p == 0 = let (count, rest) = countDivs m p
                         in (p, count) : factor rest ps
      | otherwise      = factor m ps
    countDivs m p
      | m `mod` p /= 0 = (0, m)  -- base case
      | otherwise  = let (c, r) = countDivs (m `div` p) p
                     in (c + 1, r)