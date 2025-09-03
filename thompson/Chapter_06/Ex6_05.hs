module Chapter_06.Ex6_05 where

import Chapter_06.Ex6_04

-----------------------------------------------------------
-- Exercise 6.5

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine xs ys = 
  [superimposeChar x y | (x,y) <- zip xs ys ]


-----------------------------------------------------------

{-

Testing in GHCi 

ghci> :set -i..
ghci> :load Ex6_05
ghci> superimposeLine ".##." ".#.#"
".###"


-}