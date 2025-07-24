-----------------------------------------------------------
-- Exercise 7.10

-- takes two functions of the same typing and an input list
-- generates a new list by alternately applying the two
-- functions to the elements in the input
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = 
  [if even i then f x else g x | (x, i) <- zip xs [0..]]

-- doubles a digit and subtracts 9 if the result is > 9
luhnDbl :: Int -> Int
luhnDbl x = if d <= 9 then d else d - 9
  where 
    d = 2 * x

-- applies the Luhn algorithm to a list of digits
-- returns True if the sum of the digits is divisible by 10
-- (i.e. the number is valid according to the Luhn 
-- algorithm)
luhn :: [Int] -> Bool
luhn xs = (sumDigits `mod` 10) == 0
  where 
    sumDigits = sum (altMap id luhnDbl (reverse xs))

-- point-free definition:
luhn' :: [Int] -> Bool
luhn' = (== 0) 
      . (`mod` 10) 
      . sum 
      . altMap id luhnDbl 
      . reverse

-----------------------------------------------------------