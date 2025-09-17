import Data.Char (isDigit)

-----------------------------------------------------------
-- Exercise 7.4

-- | Extracts all the digits from a string.
digits :: String -> String
digits st = [ ch | ch <- st , isDigit ch ]

-- | Returns the first digit in a string, 
-- or '\0' if there are no digits.
firstDigit :: String -> Char
firstDigit st
  | null ds = '\0'
  | otherwise = head ds
  where ds = digits st  
  

-----------------------------------------------------------