-----------------------------------------------------------
-- Exercise 4.8

{-
  THe Luhn algorithm:
  - consider each digit as a separate number
  - moving left, double every other number starting
    from the penultimate one
  - subtract 9 from any number greater than 9
  - sum all the numbers
  - if the sum is divisible by 10, the number is valid
-}

-- doubles a digit and subtracts 9 if the result is > 9
luhnDbl :: Int -> Int
luhnDbl x = if d <= 9 then d else d - 9
  where 
    d = 2 * x

-- takes for digits and checks if the 4-digit number is 
-- a valid bank car number
luhn :: Int -> Int -> Int -> Int -> Bool 
luhn a b c d = 
  (luhnDbl a + b + luhnDbl c + d) `mod` 10 == 0 

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-8.hs
  ghci> luhn 1 7 8 4
  True
  luhn 4 7 8 3
  False

-}