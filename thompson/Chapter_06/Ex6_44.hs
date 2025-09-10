import Chapter_06.Ex6_41 (formatLines)
import Chapter_06.Ex6_42 (makeTotal)
import Chapter_06.Ex6_43 (formatTotal)

-----------------------------------------------------------
-- Exercise 6.44

type Name = String
type Price = Int  -- in pence
type BarCode = Int
type TillType = [BarCode]
type BillType = [(Name,Price)]

-- | Pretty prints a bill
formatBill :: BillType -> String 
formatBill its = formatLines its 
                 ++ formatTotal (makeTotal its)

-- | Example list of items
items :: BillType
items =
  [("Dry Sherry, 1lt",540),("Fish Fingers",121),
  ("Orange Jelly",56),("Hula Hoops (Giant)",133),
  ("Unknown Item",0),("Dry Sherry, 1lt",540)]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_44

ghci> putStrLn $ formatBill items

Dry Sherry, 1lt...........5.40
Fish Fingers..............1.21
Orange Jelly..............0.56
Hula Hoops (Giant)........1.33
Unknown Item..............0.00
Dry Sherry, 1lt...........5.40

Total....................13.90

-}