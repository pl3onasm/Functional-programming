import Chapter_05.Ex5_23

-----------------------------------------------------------
-- Exercise 5.24

lineLength :: Integer
lineLength = 12

pushRight :: String -> String
pushRight s = duplicate " " (lineLength - len s) ++ s

pushRight' :: String -> Integer -> String
pushRight' s n = duplicate " " (n - len s) ++ s

len :: String -> Integer
len s = sum [1 | _ <- s]


-----------------------------------------------------------

{-

We use the function duplicate from Exercise 5.23 to create
the required number of spaces to push the string s to the
right. The function len counts the number of characters in
the string s. We needed to define len because the standard
function length returns an Int, whereas here we want an
Integer. Of course we could have used fromIntegral to 
convert but the exercises are about practising list
comprehensions.


Testing in GHCi

ghci> :set -i... 
ghci> :l Ex5_24
ghci> pushRight "crocodile"
"   crocodile"
ghci> pushRight' "crocodile" 12
"   crocodile"
ghci> pushRight' "crocodile" 5
"crocodile"


-}