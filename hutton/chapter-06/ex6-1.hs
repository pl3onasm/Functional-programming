-----------------------------------------------------------
-- Exercise 6.1

-- recursivve definition for the factiorial function
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- If applied to a negative number, this function will
-- end up in an infinite loop, since the recursive call
-- will never reach the base case (0).

-- new definition to exclude negative numbers
fac' :: Int -> Int
fac' n
  | n < 0     = error "Negative argument to fac'"
  | n == 0    = 1
  | otherwise = n * fac' (n - 1) 

-----------------------------------------------------------