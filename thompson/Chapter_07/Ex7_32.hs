import Chapter_07.Ex7_29 (splitLines, splitWords, text)
import Chapter_07.Ex7_31 (joinLines)

-----------------------------------------------------------
-- Exercise 7.32

-- | Counts the number of characters, words and lines
-- in a given string, returning a triple of these counts
wc :: String -> (Int, Int, Int)
wc st = (numChars, numWords, numLines)
  where
    words = splitWords st
    lines = splitLines words
    numLines = length lines
    numWords = length words
    numChars = length st

-- | Counts the number of characters, words and lines
-- in a given string, after justifying the text 
wcFormat :: String -> (Int, Int, Int)
wcFormat st = (numChars, numWords, numLines)
  where
    words = splitWords st
    lines = splitLines words
    numLines = length lines
    numWords = length words
    numChars = length (joinLines lines)


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :load Ex7_32
ghci> wc text
(441,79,14)
ghci> wcFormat text
(474,79,14)

The difference in character count is due to the added 
spaces for justification in wcFormat. Obviously, wcFormat 
does not alter the number of words or lines.

-}