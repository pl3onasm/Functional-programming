-----------------------------------------------------------
-- Exercise 3.17

-- | converts a digit character to its integer value
charToNum :: Char -> Int
charToNum ch
  | ch < '0' || ch > '9' = 0
  | otherwise            = fromEnum ch - fromEnum '0'

-----------------------------------------------------------