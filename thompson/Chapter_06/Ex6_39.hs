module Chapter_06.Ex6_39 where

-----------------------------------------------------------
-- Exercise 6.39

type Price = Int    -- in pence

-- | Formats a total price in pence as pounds and pence
formatPence :: Price -> String
formatPence p = pounds ++ "." ++ pence
  where
    pounds = show (p `div` 100)
    pence  = let p' = p `mod` 100
             in if p' < 10 then '0' : show p' else show p'


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_39
ghci> formatPence 12345
"123.45"
ghci> formatPence 50
"0.50"
ghci> formatPence 5
"0.05"
ghci> formatPence 0
"0.00"
ghci> formatPence 100
"1.00"
ghci> formatPence 1202
"12.02"

-}