import Data.Char (toLower, isAlpha)

-----------------------------------------------------------
-- Exercise 7.33

-- | Tests whether a string is a palindrome. 
-- Punctuation and white space are ignored, and there is no
-- distinction between capital and small letters.
isPalin :: String -> Bool
isPalin str = cleaned == reverse cleaned
  where cleaned = [toLower ch | ch <- str, isAlpha ch]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex7_33
ghci> isPalin "Madam, I'm Adam!"
True
ghci> isPalin "!R-o-tatOr....."
True
ghci> isPalin "hello"
False
ghci> isPalin "Step on no pets."
True
ghci> isPalin "Was it a car or a cat I saw?"
True

-}