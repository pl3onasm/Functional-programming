-----------------------------------------------------------
-- Exercise 7.3

-- | Returns the first integer in a list plus one, if there
-- is one, otherwise returns zero.
firstPlusOne :: [Int] -> Int
firstPlusOne xs = if null xs then 0 else head xs + 1

-- | Adds together the first two integers in a list; 
-- returns the head element if the list contains one,
-- and returns zero otherwise.
addFirstTwo :: [Int] -> Int
addFirstTwo xs =
  if null xs then 0
  else if null (tail xs) then head xs
  else head xs + head (tail xs)


-----------------------------------------------------------