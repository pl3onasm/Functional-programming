-----------------------------------------------------------
-- Exercise 3.16

-- | the difference between the ASCII codes of 'a' and 'A'
offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

-- | converts a lowercase letter to uppercase
toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)

-- | converts lowercase letters to uppercase, 
-- leaves other characters unchanged
uppercase :: Char -> Char
uppercase ch
  | ch >= 'a' && ch <= 'z' = toUpper ch
  | otherwise              = ch

-----------------------------------------------------------