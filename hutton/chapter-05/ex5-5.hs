-----------------------------------------------------------
-- Exercise 5.5

-- returns the list of all Pythagorean triples whose 
-- components are at most a given limit n
pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x, y, z) | x <- [1..n], y <- [1..n], 
                        z <- [1..n], x^2 + y^2 == z^2]

-- the former definition includes duplicates. If we only
-- want to return a list containing unique Pythagorean
-- triples, we just need to change the lower bounds of
-- the generators
upyths :: Int -> [(Int,Int,Int)]
upyths n = [ (x, y, z) | x <- [1..n], y <- [x..n], 
                        z <- [y..n], x^2 + y^2 == z^2]

-----------------------------------------------------------

{- Examples

  ghci> :l ex5-5.hs
  ghci> pyths 20

  [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),
   (8,15,17),(9,12,15),(12,5,13),(12,9,15),
   (12,16,20),(15,8,17),(16,12,20)]

  ghci> upyths 20

  [(3,4,5),(5,12,13),(6,8,10),
  (8,15,17),(9,12,15),(12,16,20)]

-}