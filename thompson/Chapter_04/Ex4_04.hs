import Chapter_03.Ex3_11 (fourEqual)
import Chapter_04.Ex4_03 (howManyEqual)

-----------------------------------------------------------
-- Exercise 4.4

-- | Determines how many of the four input values are equal
howManyOfFourEqual :: Integer -> Integer -> Integer 
                      -> Integer -> Integer
howManyOfFourEqual a b c d
  | fourEqual a b c d   = 4
  | hasNEqual 3 a b c d = 3
  | hasNEqual 2 a b c d = 2
  | otherwise           = 0

-- | Checks if any triple or pair equals exactly n elements
hasNEqual :: Integer -> Integer -> Integer -> Integer 
             -> Integer -> Bool
hasNEqual n a b c d =
  (howManyEqual a b c == n) ||
  (howManyEqual a b d == n) ||
  (howManyEqual a c d == n) ||
  (howManyEqual b c d == n)


-----------------------------------------------------------

{-

Testing in GHCi
ghci> :set -i.. 
ghci> :l Ex4_04 
ghci> howManyOfFourEqual 2 3 4 5
0
ghci> howManyOfFourEqual 2 3 2 5
2
ghci> howManyOfFourEqual 2 2 5 2
3
ghci> howManyOfFourEqual 2 2 2 2
4

-}