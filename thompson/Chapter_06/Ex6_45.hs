module Chapter_06.Ex6_45 where

-----------------------------------------------------------
-- Exercise 6.45

type Database = [(BarCode, Name, Price)]
type BarCode  = Int
type Name     = String
type Price    = Int  -- in pence

-- | Looks up a barcode in the database
look :: Database -> BarCode -> (Name, Price)
look db code
  | null pair = ("Unknown Item", 0)
  | otherwise = head pair
  where pair = [(n,p) | (c,n,p) <- db, c == code]


-----------------------------------------------------------