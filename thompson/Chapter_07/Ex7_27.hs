module Chapter_07.Ex7_27 where

import Prelude hiding (Word)

-----------------------------------------------------------
-- Exercise 7.27

type Word = String
type Line = [Word]

-- | Drops words from the start of a list such that the 
-- total length of the dropped words (plus spaces between 
-- them) does not exceed the given line length
dropLine :: Int -> [Word] -> Line
dropLine _ [] = []
dropLine lineLen (w : ws)
  | length w > lineLen = w : ws
  | otherwise          = dropLine newLen ws
  where newLen = lineLen - length w - 1 -- -1 for space


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :load Ex7_27
ghci> dropLine 10 ["never", "gonna", "give", "you", "up"]
["gonna","give","you","up"]
ghci> dropLine 15 ["never", "gonna", "say", "goodbye"]
["goodbye"]
ghci> dropline 5 ["never", "gonna", "let", "you", "down"]
["never","gonna","let","you","down"]


-}