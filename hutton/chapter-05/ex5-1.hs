-----------------------------------------------------------
-- Exercise 5.1

-- computes the sum of the first 100 integer squares
-- using a list comprehension

sqsum :: Int 
sqsum = sum [x^2 | x <- [1..100]]


-- we can make this more general, and define a function
-- that takes an int, and then produces the sum of all
-- integer squares up until the input number
sqssum :: Integer -> Integer
sqssum n = sum [x^2 | x <- [1..n]]

-- but we also have a formula for that:
sqssum' :: Integer -> Integer
sqssum' n = n * (n + 1) * (2 * n + 1) `div` 6

-----------------------------------------------------------

{- Examples:

  ghci> :l ex5-1.hs
  ghci> sqsum
  338350
  ghci> sqssum 250
  5239625

-}