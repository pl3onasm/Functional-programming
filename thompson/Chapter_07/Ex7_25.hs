-----------------------------------------------------------
-- Exercise 7.25

-- | Checks whether xs is a sublist of ys
isSublist :: String -> String -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist (x : xs) (y : ys)
  | x == y    = isSublist xs ys       
  | otherwise = isSublist (x : xs) ys   

-- | Checks whether xs is a subsequence of ys 
isSubseq :: String -> String -> Bool
isSubseq [] _ = True
isSubseq _ [] = False
isSubseq xs@(x : _) ys@(y : ys')
  | x == y    = prefixOf xs ys || isSubseq xs ys'
  | otherwise = isSubseq xs ys'
  where
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (a : as) (b : bs) = a == b && prefixOf as bs


-----------------------------------------------------------

{-

The first function, isSublist, checks whether the first
string's characters occur in the second string in the same
order, but not necessarily consecutively. 

The second function, isSubseq, checks whether the first
string's characters occur in the second string in the same
order and consecutively. That is, whether the first string
is a substring of the second string. This is done by
checking whether the first string is a prefix of any suffix
of the second string.


Testing in GHCi

ghci> :load Ex7_25.hs
ghci> isSublist "ship" "Fish & Chips"
True
ghci> isSublist "ship" "hippies"
False
ghci> isSubseq "Chip" "Fish & Chips"
True
ghci> isSubseq "Chip" "Chin up"
False

-}