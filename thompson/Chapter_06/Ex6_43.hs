module Chapter_06.Ex6_43 where

import Chapter_06.Ex6_39 

-----------------------------------------------------------
-- Exercise 6.43

lineLength :: Int
lineLength = 30

-- | Pretty prints a total
formatTotal :: Price -> String
formatTotal total = "\nTotal" ++ replicate n '.' ++ t 
  where n = lineLength - length t - 5
        t = formatPence total


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_43
ghci> formatTotal 661
"\nTotal......................661"


-}