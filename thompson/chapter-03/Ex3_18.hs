-----------------------------------------------------------
-- Exercise 3.18

-- | takes three strings and returns a single string which
-- when printend shows them on separate lines
threeLines :: String -> String -> String -> String
threeLines x y z = x ++ "\n" ++ y ++ "\n" ++ z ++ "\n"

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex3-18
ghci> threeLines "first" "second" "third"
"first\nsecond\nthird"
ghci> putStr (threeLines "first" "second" "third")
first
second
third

-}