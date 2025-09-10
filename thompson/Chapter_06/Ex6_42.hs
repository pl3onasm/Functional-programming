module Chapter_06.Ex6_42 where

-----------------------------------------------------------
-- Exercise 6.42

type Price = Int
type Name = String
type BillType = [(Name,Price)]

-- | Returns the total price of a list of items
makeTotal :: BillType -> Price
makeTotal its = sum [price | (_, price) <- its]

-- | Example list of items
items :: BillType
items =
  [("Dry Sherry, 1lt",540),("Fish Fingers",121),
  ("Orange Jelly",56),("Hula Hoops (Giant)",133),
  ("Unknown Item",0),("Dry Sherry, 1lt",540)]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_42
ghci> makeTotal items
1390
ghci> makeTotal [(" ... ",540),(" ... ",121)]
661

-}