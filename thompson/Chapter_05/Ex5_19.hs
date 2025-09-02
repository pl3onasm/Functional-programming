import Data.Char

-----------------------------------------------------------
-- Exercise 5.19

capitalize :: String -> String
capitalize cs = 
  [if isLower c then toUpper c else c | c <- cs] 

capitalizeLetters :: String -> String
capitalizeLetters cs = [toUpper c | c <- cs, isLower c]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_19
ghci> capitalize ['5', 'a', '#', '\n', 'f']
"5A#\nF"
ghci> capitalizeLetters ['5', 'a', '#', '\n', 'f']
"AF"

-}