module Chapter_06.Ex6_23 where

type Picture = [[(Int, Char)]]

-----------------------------------------------------------
-- Exercise 6.23

-- | Prints a picture
printPicture :: Picture -> IO ()
printPicture pic = putStr $ unlines 
  [[c | (n,c) <- line, _ <- [1..n] ] | line <- pic]

-- | Pads a picture with '.' to make it rectangular
padToRect :: Picture -> Picture
padToRect [] = []
padToRect pic = [pad line | line <- pic]
  where
    pad line 
      | extra <= 0 = line
      | last line == (n, '.') 
          = init line ++ [(n + extra, '.')]
      | otherwise = line ++ [(extra, '.')]
      where
        extra = maxLen - lineLen line
        (n, _) = last line
    lineLen line = sum [n | (n, _) <- line]
    maxLen = maximum [lineLen ln | ln <- pic]

-- | Stacks two pictures vertically
above :: Picture -> Picture -> Picture
above pic1 pic2 = padToRect (pic1 ++ pic2)

-- | Gets the height of a picture
height :: Picture -> Int
height pic = length pic

-- | Gets the width of a picture
width :: Picture -> Int
width pic = maximum [sum [n | (n,_) <- line] | line <- pic]

-- | Pads two pictures to the same height
padToSameHeight :: Picture -> Picture -> (Picture, Picture)
padToSameHeight pic1 pic2
  | h1 < h2   = (padToRect (pic1 ++ 
                 replicate (h2 - h1) [(w1, '.')]), pic2)
  | h1 > h2   = (pic1, padToRect (pic2 ++ 
                 replicate (h1 - h2) [(w2, '.')]))
  | otherwise = (pic1, pic2)
  where
    h1 = height pic1
    h2 = height pic2
    w1 = width pic1
    w2 = width pic2

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside pic1 pic2 =  
  [patch line1 line2 | (line1, line2) <- zip p1 p2]
  where
    (p1, p2) = padToSameHeight pic1 pic2
    patch ln1 ln2 
      | last ln1 == (n1, '.') && head ln2 == (n2, '.')
          = init ln1 ++ [(n1 + n2, '.')] ++ tail ln2
      | otherwise = ln1 ++ ln2
      where
        (n1, _) = last ln1
        (n2, _) = head ln2
    
-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH = reverse

-- | Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV pic = [reverse line | line <- padToRect pic]

-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Scales a picture by a factor of n
scale :: Picture -> Int -> Picture
scale pic n
  | n <= 0    = []
  | otherwise = concat [rep line | line <- pic]
  where 
    rep xs = replicate n $ [(k*n, c) | (k, c) <- xs]

-- | Example picture
pic :: Picture
pic = 
  [[(1,'.'),(2,'#'),(1,'.')], 
  [(1,'.'),(1,'#'),(1,'.'),(1,'#')],
  [(1,'.'),(3,'#')],
  [(4,'#')]]

-- | Horse picture from the book
horse :: Picture
horse = 
  [[(7,'.'),(2,'#'),(3,'.')],
  [(5,'.'),(2,'#'),(2,'.'),(1,'#'),(2,'.')],
  [(3,'.'),(2,'#'),(5,'.'),(1,'#'),(1,'.')],
  [(2,'.'),(1,'#'),(7,'.'),(1,'#'),(1,'.')],
  [(2,'.'),(1,'#'),(3,'.'),(1,'#'),(3,'.'),(1,'#'),(1,'.')],
  [(2,'.'),(1,'#'),(3,'.'),(3,'#'),(1,'.'),(1,'#'),(1,'.')],
  [(1,'.'),(1,'#'),(4,'.'),(1,'#'),(2,'.'),(2,'#'),(1,'.')],
  [(2,'.'),(1,'#'),(3,'.'),(1,'#'),(5,'.')],
  [(3,'.'),(1,'#'),(3,'.'),(1,'#'),(4,'.')],
  [(4,'.'),(1,'#'),(2,'.'),(1,'#'),(4,'.')],
  [(5,'.'),(1,'#'),(1,'.'),(1,'#'),(4,'.')],
  [(6,'.'),(2,'#'),(4,'.')]]

-- | Example of non-rectangular picture
arrow :: Picture
arrow = 
  [[(2,'#')],
  [(1,'#'),(2,'.'),(1,'#')],
  [(1,'#'),(4,'.'),(1,'#')],
  [(1,'#'),(6,'.'),(1,'#')],
  [(1,'#'),(8,'.'),(1,'#')],
  [(1,'#'),(10,'.'),(1,'#')],
  [(1,'#'),(8,'.'),(1,'#')],
  [(1,'#'),(6,'.'),(1,'#')],
  [(1,'#'),(4,'.'),(1,'#')],
  [(1,'#'),(2,'.'),(1,'#')],
  [(2,'#')]]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_23
ghci> printPicture pic
.##.
.#.#
.###
####
ghci> printPicture horse
.......##...
.....##..#..
...##.....#.
..#.......#.
..#...#...#.
..#...###.#.
.#....#..##.
..#...#.....
...#...#....
....#..#....
.....#.#....
......##....
ghci> printPicture arrow
##
#..#
#....#
#......#
#........#
#..........#
#........#
#......#
#....#
#..#
##
ghci> printPicture $ pic `beside` horse
.##........##...
.#.#.....##..#..
.###...##.....#.
####..#.......#.
......#...#...#.
......#...###.#.
.....#....#..##.
......#...#.....
.......#...#....
........#..#....
.........#.#....
..........##....
ghci> printPicture $ horse `above` arrow
.......##...
.....##..#..
...##.....#.
..#.......#.
..#...#...#.
..#...###.#.
.#....#..##.
..#...#.....
...#...#....
....#..#....
.....#.#....
......##....
##..........
#..#........
#....#......
#......#....
#........#..
#..........#
#........#..
#......#....
#....#......
#..#........
##..........
ghci> printPicture $ flipV $ scale arrow 3
..............................######
..............................######
..............................######
........................###......###
........................###......###
........................###......###
..................###............###
..................###............###
..................###............###
............###..................###
............###..................###
............###..................###
......###........................###
......###........................###
......###........................###
###..............................###
###..............................###
###..............................###
......###........................###
......###........................###
......###........................###
............###..................###
............###..................###
............###..................###
..................###............###
..................###............###
..................###............###
........................###......###
........................###......###
........................###......###
..............................######
..............................######
..............................######



-}





