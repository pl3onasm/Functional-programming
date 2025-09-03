import Chapter_06.Ex6_07

-----------------------------------------------------------
-- Exercise 6.10

scale :: Picture -> Int -> Picture
scale pic n
  | n <= 0    = []
  | otherwise = concat [rep line | line <- pic]
  where 
    rep xs = replicate n $ concat [replicate n c | c <- xs]


-----------------------------------------------------------

{-

Scaling a picture by a factor of n means replacing each
character in the picture by an n x n block of that 
character. Our function achieves this by defining a local
function rep which takes a line of the picture and replaces
each character c in that line by a string of n copies of c
(horizontal scaling), and then replicates the resulting 
line n times (vertical scaling). The main function scale 
then applies rep to each line of the picture and 
concatenates the results.


Testing in GHCi

ghci> :l Ex5_10
ghci> pic = ["#.#","..#"] 
ghci> printPicture (scale pic 2)
##..##
##..##
....##
....##
ghci> pic = [".##.", ".#.#", ".###", "####"]
ghci> printPicture (scale pic 3)
...######...
...######...
...######...
...###...###
...###...###
...###...###
...#########
...#########
...#########
############
############
############



-}