module Chapter_06.Ex6_46 where

import Prelude hiding (lookup)
import Chapter_06.Ex6_45 (look)

-----------------------------------------------------------
-- Exercise 6.46

type Database = [(BarCode, Name, Price)]
type BarCode  = Int
type Name     = String
type Price    = Int  -- in pence

-- | Looks up an item in codeIndex
lookup :: BarCode -> (Name, Price)
lookup = look codeIndex

-- | Example database
codeIndex :: Database
codeIndex = 
  [(4719, "Fish Fingers" , 121),
  (5643, "Nappies" , 1010),
  (3814, "Orange Jelly", 56),
  (1111, "Hula Hoops", 21),
  (1112, "Hula Hoops (Giant)", 133),
  (1234, "Dry Sherry, 1lt", 540)]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_46
ghci> lookup 3814
("Orange Jelly",56)
ghci> lookup 9999
("Unknown Item",0)
ghci> lookup 1112
("Hula Hoops (Giant)",133)

-}