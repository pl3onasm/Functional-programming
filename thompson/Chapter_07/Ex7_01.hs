-----------------------------------------------------------
-- Exercise 7.1

-- | Returns the first integer in a list plus one, if there
-- is one, otherwise returns zero.
firstPlusOne :: [Int] -> Int
firstPlusOne []      = 0
firstPlusOne (x : _) = x + 1


-----------------------------------------------------------