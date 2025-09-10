-----------------------------------------------------------
-- Exercise 6.49

type Name = String
type Price = Int  -- in pence
type BarCode = Int
type Database = [(BarCode, Name, Price)]

-- | Removes a barcode from the database 
removeItem :: BarCode -> Database -> Database
removeItem bc db = [(b, n, p) | (b, n, p) <- db, b /= bc]

-- | Adds a barcode to the database
addItem :: BarCode -> Name -> Price -> Database -> Database
addItem bc n p db = (bc, n, p) : removeItem bc db

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
ghci> :l Ex6_49
ghci> db = removeItem 1111 codeIndex
ghci> db
[(4719,"Fish Fingers",121),(5643,"Nappies",1010),
(3814,"Orange Jelly",56),(1112,"Hula Hoops (Giant)",133),
(1234,"Dry Sherry, 1lt",540)]
ghci> db' = addItem 1234 "Mint Tea" 150 db
ghci> db'
[(1234,"Mint Tea",150),(4719,"Fish Fingers",121),
(5643,"Nappies",1010),(3814,"Orange Jelly",56),
(1112,"Hula Hoops (Giant)",133)]

-}