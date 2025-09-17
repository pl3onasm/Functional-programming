module Chapter_07.Ex7_29 where

import Chapter_07.Ex7_28 (joinLine)
import Chapter_07.Ex7_27 (dropLine)
import Prelude hiding (Word, getLine)

-----------------------------------------------------------
-- Exercise 7.29

type Word = String
type Line = [Word]

-- | Joins a list of lines into a single string, separating
-- lines with newline characters
joinLines :: [Line] -> String
joinLines []       = ""
joinLines [l]      = joinLine l
joinLines (l : ls) = joinLine l ++ "\n" ++ joinLines ls 

-- | Sample text to demonstrate functionality
text :: String
text = 
  "The castle seemed very quiet even for a Sunday. "
  ++ "Everybody was clearly out in the sunny grounds, "
  ++ "enjoying the end of their exams and the prospect of "
  ++ "a last few days of term unhampered by studying or "
  ++ "homework. Harry walked slowly along the deserted "
  ++ "corridor, peering out of windows as he went. He "
  ++ "could see people messing around in the air over "
  ++ "the Quidditch pitch and a couple of students "
  ++ "swimming in the lake, accompanied by the giant "
  ++ "squid." 


-----------------------------------------------------------
-- The rest of the code from the textbook

-- | Characters considered as whitespace
whitespace :: [Char]
whitespace = [' ', '\n', '\t']

-- | Maximum length of a line
lineLen :: Int
lineLen = 35

-- | Gets the next word from the start of a string
getWord :: String -> String
getWord [] = [] 
getWord (x:xs)
  | elem x whitespace = [] 
  | otherwise = x : getWord xs 

-- | Drops characters from the start of a string until
-- a whitespace character is found
dropWord :: String -> String
dropWord [] = []
dropWord (x : xs)
  | elem x whitespace = (x : xs)
  | otherwise = dropWord xs

-- | Drops whitespace characters from the start of a string
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x : xs)
  | elem x whitespace = dropSpace xs
  | otherwise = (x : xs)

-- | Splits a string into a list of words
splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

-- | Helper function for splitWords
split :: String -> [Word]
split [] = []
split st
  = (getWord st) : split (dropSpace (dropWord st))

-- | Gets a line of words no longer than the given length
getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w : ws)
  | length w <= len = w : restOfLine
  | otherwise = []
  where
    newlen = len - (length w + 1)
    restOfLine = getLine newlen ws

-- | Splits a list of words into lines no longer than
-- the given length
splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws
  = getLine lineLen ws : splitLines (dropLine lineLen ws)


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :load Ex7_29
ghci> words = splitWords text
ghci> lines = splitLines words
ghci> putStrLn (joinLines lines)
The castle seemed very quiet even
for a Sunday. Everybody was clearly
out in the sunny grounds, enjoying
the end of their exams and the
prospect of a last few days of term
unhampered by studying or homework.
Harry walked slowly along the
deserted corridor, peering out of
windows as he went. He could see
people messing around in the air
over the Quidditch pitch and a
couple of students swimming in the
lake, accompanied by the giant
squid.

-}