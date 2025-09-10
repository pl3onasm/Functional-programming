module Chapter_06.Ex6_41 where

import Chapter_06.Ex6_40

-----------------------------------------------------------
-- Exercise 6.41

type Price = Int

-- | Pretty prints a list of items
formatLines :: [(Name,Price)] -> String
formatLines its = concat [formatLine it | it <- its]

-- | Example list of items
items :: [(Name, Price)]
items =
  [("Dry Sherry, 1lt",540),("Fish Fingers",121),
  ("Orange Jelly",56),("Hula Hoops (Giant)",133),
  ("Unknown Item",0),("Dry Sherry, 1lt",540)]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_41

ghci> putStr $ formatLines items

Dry Sherry, 1lt...........5.40
Fish Fingers..............1.21
Orange Jelly..............0.56
Hula Hoops (Giant)........1.33
Unknown Item..............0.00
Dry Sherry, 1lt...........5.40


-}