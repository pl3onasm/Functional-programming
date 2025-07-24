import Data.Char

-----------------------------------------------------------
-- Exercise 5.10

low2int :: Char -> Int
low2int c = ord c - ord 'a'

up2int :: Char -> Int
up2int c = ord c - ord 'A'

int2low :: Int -> Char
int2low n = chr (ord 'a' + n)

int2up :: Int -> Char
int2up n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2low ((low2int c + n) `mod` 26)
  | isUpper c = int2up  ((up2int  c + n) `mod` 26)
  | otherwise = c

-- implements the Caesar cipher for lower and 
-- uppercase letters
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-----------------------------------------------------------

{- Examples

  ghci> :l ex5-10.hs
  ghci> encode 3 "Haskell is FUN!"
  "Kdvnhoo lv IXQ!"

  ghci> encode (-3) "Kdvnhoo lv IXQ!"
  "Haskell is FUN!"

-}