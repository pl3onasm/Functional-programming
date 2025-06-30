-----------------------------------------------------------
-- Exercise 18.2

-- There are many ways to define the Fibonacci sequence.
-- All these definitions rely on Haskell’s lazy evaluation  
-- to avoid computing an actual infinite list in memory.

-- 1. Using an explicit recursive helper function.
--    The helper takes two arguments representing the 
--    current and next Fibonacci numbers.
fibo :: [Integer]
fibo = fib 0 1
  where
    fib a b = a : fib b (a + b)

-- 2. Using `zip` and a list comprehension.
--    We construct an infinite list of pairs, each holding 
--    two consecutive Fibonacci numbers. The sum of each 
--    pair gives the next number in the sequence.
fiboZ :: [Integer]
fiboZ = 0 : 1 : [a + b | (a, b) <- zip fiboZ (tail fiboZ)]

-- 3. Using `zipWith` to combine the list with its tail.
--    This expresses the same concept as `fiboZ`, but 
--    more concisely.
fiboZW :: [Integer]
fiboZW = 0 : 1 : zipWith (+) fiboZW (tail fiboZW)

-- 4. Using `iterate` to repeatedly apply a shift
--    to a tuple of consecutive Fibonacci numbers.
--    We map `fst` to extract the first element from each 
--    tuple in the resulting stream.
fiboT :: [Integer]
fiboT = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

-- 5. Using `scanl` to accumulate the sequence.
--    `scanl` is like `foldl`, but returns all intermediate 
--    results. This version builds the list by prepending 0 
--    and repeatedly adding the next number to the sum.
fiboS :: [Integer]
fiboS = scanl (+) 0 (1 : fiboS)


