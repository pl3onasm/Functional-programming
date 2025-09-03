module Chapter_06.Ex6_07 where 

type Picture = [[Char]]

-----------------------------------------------------------
-- Exercise 6.7

printPicture :: Picture -> IO ()
printPicture pic = 
  putStr $ concat [line ++ "\n" | line <- pic]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_07
ghci> printPicture [".##.", ".#.#", ".###", "####"]
.##.
.#.#
.###
####

-}