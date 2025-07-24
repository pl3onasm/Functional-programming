-----------------------------------------------------------
-- Exercise 4.6

-- defines logical && using conditional expressions, but 
-- now following the pattern matching definition
--    True && b = b
--    False && _ = False
land :: Bool -> Bool -> Bool
land a b = if a then b else False

-----------------------------------------------------------

{- 
  The version in exercise 4.5 explicitly checks both 
  values of the boolean expression, and only returns True
  if both are True. That is why it has two nested ifs.

  This version, however, captures the short-circuiting
  nature of the logical AND operation. If the first value
  is False, no need to evaluate the second, otherwise
  return the second value as the result. This is why
  it only has one if statement.
-}