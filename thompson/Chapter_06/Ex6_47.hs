import Prelude hiding (lookup)
import Chapter_06.Ex6_46 (lookup)

-----------------------------------------------------------
-- Exercise 6.47

type Name = String
type Price = Int  -- in pence
type BarCode = Int
type TillType = [BarCode]
type BillType = [(Name,Price)]


makeBill :: TillType -> BillType
makeBill codes = [lookup code | code <- codes]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_47
ghci> makeBill [1234,4719,3814,1112,1113,1234]
[("Dry Sherry, 1lt",540),("Fish Fingers",121),
("Orange Jelly",56),("Hula Hoops (Giant)",133),
("Unknown Item",0),("Dry Sherry, 1lt",540)]


-}