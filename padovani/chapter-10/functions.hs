import Data.List
import Data.Char

-----------------------------------------------------------
-- Exercise 10.1

-- Shows the input list
myShowList :: (Show a) => [a] -> String
myShowList xs = "[" ++ flatten (intersperse "," lst) ++ "]"
  where 
    lst :: [String]
    lst = [show x | x <- xs]
    flatten [] = ""
    flatten (s : ss) = s ++ flatten ss

-- But, there already exists a function in the Prelude
-- for flatten, namely concat, so we can use that
showLst :: (Show a) => [a] -> String
showLst xs = "[" ++ concat (intersperse "," lst) ++ "]"
  where 
    lst = [show x | x <- xs]

-- Note that (intersperse ',' lst) would produce a type
-- error, since the list would then contain two different 
-- types: String and Char, which is ill-typed

-----------------------------------------------------------
-- Exercise 10.2

readHexDigit :: Char -> Int
readHexDigit d 
  | d >= '0' && d <= '9' = ord d - ord '0'
  | d >= 'A' && d <= 'F' = ord d - ord 'A' + 10
  | d >= 'a' && d <= 'f' = ord d - ord 'a' + 10

-- This edit would not only accept inputs like " 4de" and
-- "4de  \n", but also things like "4  d e"
readHexInt :: String -> Int
readHexInt s = aux 0 (clean s)
  where
    clean s = [c | c <- s, not (isSpace c)]
    aux x "" = x
    aux x (c : cs) = aux (16 * x + readHexDigit c) cs

-- This edit only removes space from the start and/or end
-- of the input string, so "4  d e" would produce an error
readHexInt' :: String -> Int
readHexInt' s = aux 0 (trim s)
  where
    aux x "" = x
    aux x (c : cs) = aux (16 * x + readHexDigit c) cs
    trim s = trimEnd (trimStart s)
    trimStart [] = []
    trimStart (c : cs)
      | isSpace c = trimStart cs
      | otherwise = c : cs
    trimEnd s = reverse $ trimStart $ reverse s

-----------------------------------------------------------
-- Exercise 10.3

-- Splits an input string into a list of words (which are 
-- substrings separated by spaces)
splitWords :: String -> [String]
splitWords s = [w | w <- collectWords s, w /= ""]
  where 
    collectWords [] = [""]
    collectWords (c : cs)
      | isSpace c = "" : collectWords cs
      | otherwise = let w : ws = collectWords cs
                    in (c : w) : ws

-----------------------------------------------------------
-- Exercise 10.4

-- Takes a list of words, and concatenates the words into
-- a sentence where they are separated by spaces
joinWords :: [String] -> String
joinWords ws = concat (intersperse " " ws)

-- If we want to avoid using intersperse, we can do this:
joinWords' :: [String] -> String
joinWords' (w1 : ws) = w1 ++ concat [" " ++ w | w <- ws]

-----------------------------------------------------------
-- Exercise 10.5

-- Converts a string into a fractional number;
-- int and frac stand for the integral and fractional part
-- of the number, div stands for divisor
strToDouble :: String -> Double
strToDouble s = parse s 0 0 False 1
  where
    parse [] int frac _ _ = int + frac
    parse (c : cs) int frac dot div
      | c == '.' = parse cs int frac True 1
      | dot = parse cs int (frac + d / (div * 10)) 
              True (div * 10)
      | otherwise =  parse cs (int * 10 + d) frac False div
      where d = fromIntegral (ord c - ord '0') :: Double

-----------------------------------------------------------