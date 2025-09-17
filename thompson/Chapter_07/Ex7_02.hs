-----------------------------------------------------------
-- Exercise 7.2

-- | Adds together the first two integers in a list; 
-- returns the head element if the list contains one,
-- and returns zero otherwise.
addFirstTwo :: [Int] -> Int
addFirstTwo []  = 0
addFirstTwo [x] = x
addFirstTwo (x : y : _) = x + y


-----------------------------------------------------------