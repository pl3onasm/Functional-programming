module Chapter_07.Ex7_28 where

import Prelude hiding (Word)

-----------------------------------------------------------
-- Exercise 7.28

type Word = String
type Line = [Word]

-- | Joins a line (a list of words) into a single string,
-- separating words with spaces
joinLine :: Line -> String
joinLine []       = ""
joinLine [w]      = w
joinLine (w : ws) = w ++ " " ++ joinLine ws


-----------------------------------------------------------

{-

Testing in GHCi 

ghci> :load Ex7_28
ghci> joinLine ["never", "gonna", "give", "you", "up"]
"never gonna give you up"

-}