import Chapter_07.Ex7_28 (joinLine)
import Chapter_07.Ex7_29 (joinLines, text, lineLen, 
                          whitespace, Line, Word)
import Prelude hiding (Word)

----------------------------------------------------------
-- Exercise 7.30

-- | Splits a string into a (word, restOfString) pair
splitWord :: String -> (Word, String)
splitWord [] = ([], [])
splitWord (x : xs)
  | elem x whitespace = splitWord xs  -- skip whitespace
  | otherwise = buildWord [x] xs
  where
    buildWord acc [] = (acc, [])
    buildWord acc (y : ys)
      | elem y whitespace = (reverse acc, y : ys)
      | otherwise         = buildWord (y : acc) ys

-- | Splits a string into a list of words by repeatedly
-- applying splitWord
splitWords :: String -> [Word]
splitWords [] = []
splitWords st = case splitWord st of
                  ([], _)   -> []
                  (w, rest) -> w : splitWords rest

-- | Splits a list of words into a (line, restOfWords) pair
splitLine :: Int -> [Word] -> (Line, [Word])
splitLine _ [] = ([], [])
splitLine n (w : ws)
  | length w > n = ([], w : ws)
  | otherwise = (w : line, rest)
  where
    (line, rest) = splitLine (n - length w - 1) ws

-- | Splits a list of words into a list of lines by
-- repeatedly applying splitLine
splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = line : splitLines rest
  where
    (line, rest) = splitLine lineLen ws


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :load Ex7_30
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