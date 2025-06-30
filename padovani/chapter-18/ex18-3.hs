-----------------------------------------------------------
-- Exercise 18.3

-- generates the infinite list of prime numbers
-- using a sieve, which filters out multiples of each prime
-- starting from 2.
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (x : xs) = x : sieve [n | n <- xs, mod n x /= 0]

-- computes the prime factorization of a number using the
-- infinite list of primes and trial division. The upper
-- bound is given by the square root of the input number.
-- For example, factors 72 results in [2,2,2,3,3]
factors :: Integer -> [Integer]
factors n = fact n primes
  where
    up = floor . sqrt . fromIntegral
    fact m (p : ps)
      | p > up m = []
      | m `mod` p == 0 = p : fact (m `div` p) (p : ps)
      | otherwise = fact m ps

-- groups consecutive elements in a list that are equal
-- into sublists. For example, group [1,1,2,3,3,3,4]
-- results in [[1,1],[2],[3,3,3],[4]]
group :: Eq a => [a] -> [[a]]
group [] = []
group (x : xs) = grp [x] xs
  where
    grp current [] = [current]
    grp current@(c : _) (y : ys)
      | c == y    = grp (y : current) ys  
      | otherwise = current : grp [y] ys  

-- generates the prime factorization of a number
-- as a list of pairs, where each pair contains a prime
-- factor and its exponent in the factorization
-- For example, factorize 18 results in [(2,1),(3,2)]
-- meaning 18 = 2^1 * 3^2.
factorize :: Integer -> [(Integer, Int)]
factorize = map (\xs -> (head xs, length xs)) 
            . group . factors
