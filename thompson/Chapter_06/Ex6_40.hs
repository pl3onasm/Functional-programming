module Chapter_06.Ex6_40 where

import Chapter_06.Ex6_39

-----------------------------------------------------------
-- Exercise 6.40

type Name = String

lineLength :: Int
lineLength = 30

-- | Pretty prints a line: item name and price
formatLine :: (Name, Price) -> String
formatLine (name, pence) = 
  name ++ replicate n '.' ++ price ++ "\n"
  where 
    n = lineLength - length (name++price)
    price = formatPence pence


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_40
ghci> formatLine ("Eggs", 239)
"Eggs......................2.39\n"
ghci> formatLine ("Dry Sherry, 1lt", 540)
"Dry Sherry, 1lt...........5.40\n"


-}