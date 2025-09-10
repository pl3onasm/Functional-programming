import Chapter_06.Ex6_50 (makeBill)
import Chapter_06.Ex6_39 (formatPence)
import Chapter_06.Ex6_40 (formatLine, lineLength)
import Data.List (sort, sortOn)

-----------------------------------------------------------
-- Exercise 6.52

type Name = String 
type Price = Int -- in pence 
type BarCode = Int 
type TillType = [BarCode] 
type BillType = [(Name,Price)] 
type Database = [(BarCode, Name, Price)]


-- | Add one sale into the table of total sales
addSale :: (Name, Price) -> [(Name, Price)] -> [(Name, Price)]
addSale (n,p) [] = [(n,p)]
addSale (n,p) ((m, q) : rest)
  | n == m    = (m, q + p) : rest
  | otherwise = (m,q) : addSale (n,p) rest

-- | Add all sales from one bill into a table
addBill :: BillType -> [(Name, Price)] -> [(Name, Price)]
addBill [] acc     = acc
addBill (x : xs) acc = addBill xs (addSale x acc)

-- | Compute total sales across all tills
salesTotals :: [TillType] -> [(Name, Price)]
salesTotals []       = []
salesTotals (bs : bss) = 
  addBill (makeBill bs) (salesTotals bss)

-- | Pretty-print a table of total sales, including the 
-- grand total on the last line
formatSalesTotals :: [TillType] -> String
formatSalesTotals bss = "\nTotal Sales\n\n" ++ concat (
  sort [formatLine item | item <- totals]) ++ bar 
  ++ "\n" ++ grandTotal
  where grand = sum [p | (_,p) <- totals]
        bar = replicate lineLength '_' ++ "\n"
        grandTotal = formatLine ("Grand Total", grand)
        totals = salesTotals bss

-- | Given a list of items, returns all unique pairs of 
-- items
pairs :: Ord a => [a] -> [(a,a)]
pairs []       = []
pairs (x : xs) = 
  [(min x y, max x y) | y <- xs, y /= x] ++ pairs xs

-- | Add one pair into the table of co-purchases
addPair :: (Name,Name) -> [((Name,Name), Int)] 
            -> [((Name,Name), Int)]
addPair p [] = [(p,1)]
addPair (a,b) (((x,y), n) : rest)
  | (a,b) == (x,y) = ((x,y), n + 1) : rest
  | otherwise      = ((x,y), n) : addPair (a,b) rest

-- | Add all pairs into the table of co-purchases
addPairs :: [(Name,Name)] -> [((Name,Name), Int)] 
            -> [((Name,Name), Int)]
addPairs [] acc     = acc
addPairs (p : ps) acc = addPairs ps (addPair p acc)

-- | Count co-purchases across all tills
coPurchases :: [TillType] -> [((Name,Name), Int)]
coPurchases []       = []
coPurchases (bs:bss) =
  let items = [n | (n,_) <- makeBill bs]
      billPairs = pairs items
  in addPairs billPairs (coPurchases bss)

-- | Pretty-print a table of co-purchases of items across
-- all tills, sorted in descending order of frequency
formatCoPurchases :: [TillType] -> String
formatCoPurchases bss = "\nCo-Purchases\n\n" ++ concat 
  [show n ++ " x " ++ a ++ " & " ++ b ++ "\n"
  | ((a,b), n) <- sorted]
    where sorted = reverse (sortOn snd (coPurchases bss))

-- | Example sales data 
sales :: [TillType]
sales =
  [ [1234, 4719, 3814], 
    [4719, 3814, 5643, 1112], 
    [1234, 4719, 5643],
    [1111, 1113, 1234],
    [4719, 3814, 4719, 1112],
    [1113, 1234, 4719, 3814, 1111],
    [1111, 5643, 1113],
    [1112, 1112, 1113],
    [3814, 1113, 4719],
    [1111, 1112, 1113, 1234],
    [1111, 5643, 1111, 3814, 3814]           
  ]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_52
ghci> putStrLn $ formatSalesTotals sales

Total Sales

Dry Sherry, 1lt..........27.00
Fish Fingers..............8.47
Hula Hoops (Giant)........6.65
Hula Hoops................1.26
Nappies..................40.40
Orange Jelly..............3.92
______________________________

Grand Total..............87.70

ghci> putStrLn $ formatCoPurchases sales

Co-Purchases

6 x Fish Fingers & Orange Jelly
5 x Hula Hoops & Orange Jelly
3 x Fish Fingers & Hula Hoops (Giant)
3 x Dry Sherry, 1lt & Fish Fingers
3 x Dry Sherry, 1lt & Hula Hoops
3 x Nappies & Orange Jelly
3 x Hula Hoops & Nappies
2 x Fish Fingers & Nappies
2 x Hula Hoops (Giant) & Orange Jelly
2 x Dry Sherry, 1lt & Orange Jelly
1 x Hula Hoops (Giant) & Nappies
1 x Dry Sherry, 1lt & Nappies
1 x Fish Fingers & Hula Hoops
1 x Dry Sherry, 1lt & Hula Hoops (Giant)
1 x Hula Hoops & Hula Hoops (Giant)


-}