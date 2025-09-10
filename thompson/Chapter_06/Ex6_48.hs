import Chapter_06.Ex6_43 (formatTotal, lineLength)
import Chapter_06.Ex6_39 (formatPence)
import Chapter_06.Ex6_42 (makeTotal, items)
import Chapter_06.Ex6_41 (formatLines)

-----------------------------------------------------------
-- Exercise 6.48

type Name = String
type Price = Int  -- in pence
type BarCode = Int
type TillType = [BarCode]
type BillType = [(Name,Price)]

-- | Returns the total price of a list of items
makeDiscount :: BillType -> Price
makeDiscount its 
  | s == 0    = 0
  | otherwise = s `div` 2
  where  
  s = sum [100 | (it, _) <- its, it == "Dry Sherry, 1lt"]

-- | Pretty prints a discount
formatDiscount :: Price -> String
formatDiscount discount = 
  "\nDiscount" ++ replicate n '.' ++ disc ++ "\n"
  where n = lineLength - length disc - 8
        disc = formatPence discount

-- | Pretty prints a bill
formatBill :: BillType -> String 
formatBill its = 
  formatLines its ++ discount 
  ++ formatTotal (makeTotal its - makeDiscount its)
  where discount = if d == 0 then "" else formatDiscount d
        d = makeDiscount its


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_48
ghci> putStrLn $ formatBill items
Dry Sherry, 1lt...........5.40
Fish Fingers..............1.21
Orange Jelly..............0.56
Hula Hoops (Giant)........1.33
Unknown Item..............0.00
Dry Sherry, 1lt...........5.40

Discount..................1.00

Total....................12.90


-}