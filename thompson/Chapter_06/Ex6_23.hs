module Chapter_06.Ex6_23 where

-----------------------------------------------------------
-- Exercise 6.23

-- | A picture is a list of lines, each line being a
-- run-length encoding, i.e. a list of pairs (n,c)
-- meaning that character c is repeated n times.
type Picture = [[(Int, Char)]]

-- | Prints a picture
printPicture :: Picture -> IO ()
printPicture pic = putStr $ unlines 
  [[c | (n,c) <- line, _ <- [1..n]] | line <- pic]

-- | Encodes a list of strings into a picture
toPicture :: [String] -> Picture
toPicture rows = [rle row | row <- rows]

-- | Inverts the color of a picture
invertColor :: Picture -> Picture
invertColor pic = 
  [[(n, swap c) | (n,c) <- line] | line <- pic]

-- | Swap '.' and '#'.
swap :: Char -> Char
swap '.' = '#'
swap '#' = '.'
swap  c  = error $ "Invalid char in picture: " ++ [c]

-- | Run-length encodes a string
rle :: String -> [(Int, Char)]
rle []       = []
rle (x : xs) = encode 1 x xs
  where
    encode n c [] = [(n,c)]
    encode n c (y : ys)
      | y == c    = encode (n+1) c ys
      | otherwise = (n,c) : encode 1 y ys

-- | Pads a picture with '.' to make it rectangular
padToRect :: Picture -> Picture
padToRect [] = []
padToRect pic = [pad line | line <- pic]
  where
  pad line 
    | extra <= 0 = line
    | lastPair == (n,'.') = init line ++ [(n + extra, '.')]
    | otherwise = line ++ [(extra, '.')]
    where 
      extra = width pic - lineLen line
      lastPair@(n, c) = last line
      lineLen line = sum [n | (n, _) <- line]

-- | Stacks two pictures vertically
above :: Picture -> Picture -> Picture
above pic1 pic2 = padToRect (pic1 ++ pic2)

-- | Gets the height of a picture
height :: Picture -> Int
height pic = length pic

-- | Gets the width of a picture
width :: Picture -> Int
width pic = maximum [sum [n | (n,_) <- line] | line <- pic]

-- | A blank picture
blank :: Picture
blank = [[(1, '.')]]

-- | Pads two pictures to the same height
padToSameHeight :: Picture -> Picture -> (Picture, Picture)
padToSameHeight [] pic2 = (blank, padToRect pic2)
padToSameHeight pic1 [] = (padToRect pic1, blank)
padToSameHeight pic1 pic2
  | h1 < h2   = (padToRect (pic1 ++ 
                 replicate (h2 - h1) [(w1, '.')]), pic2)
  | h1 > h2   = (pic1, padToRect (pic2 ++ 
                 replicate (h1 - h2) [(w2, '.')]))
  | otherwise = (pic1, pic2)
  where
    (h1, h2) = (height pic1, height pic2)
    (w1, w2) = (width pic1, width pic2)

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside [] pic2 = pic2
beside pic1 [] = pic1
beside pic1 pic2 = [patch line1 line2 | 
                    (line1, line2) <- zip l r]
  where
    (l, r) = (padToRect p1, padToRect p2)
    (p1, p2) = padToSameHeight pic1 pic2
    patch ln1 ln2 
      | c1 /= c2  = ln1 ++ ln2
      | otherwise = init ln1 ++ [(n1 + n2, c1)] ++ tail ln2
      where
        (n1, c1) = last ln1
        (n2, c2) = head ln2
    
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
lambda :: Picture
lambda = 
  [[(14,'.')],[(2,'.'),(2,'#'),(10,'.')],
  [(4,'.'),(1,'#'),(9,'.')],
  [(5,'.'),(1,'#'),(8,'.')],
  [(5,'.'),(2,'#'),(7,'.')],
  [(4,'.'),(1,'#'),(1,'.'),(2,'#'),(6,'.')],
  [(3,'.'),(1,'#'),(2,'.'),(4,'#'),(4,'.')],
  [(2,'.'),(2,'#'),(3,'.'),(5,'#'),(2,'.')],
  [(14,'.')]]

-- | List of strings 
delta :: [String]
delta =
  ["..............",
   ".....##.......",
   "....#.........",
   ".....#........",
   ".....##.......",
   "....##.##.....",
   "...##....#....",
   "...##....#....",
   "....#####.....",
   ".............."]

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
ghci> printPicture lambda
..............
..##..........
....#.........
.....#........
.....##.......
....#.##......
...#..####....
..##...#####..
..............   
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
ghci> pic = toPicture delta
ghci> pic
[[(14,'.')],[(5,'.'),(2,'#'),(7,'.')],
[(4,'.'),(1,'#'),(9,'.')],[(5,'.'),(1,'#'),(8,'.')],
[(5,'.'),(2,'#'),(7,'.')],[(4,'.'),(2,'#'),(1,'.'),
(2,'#'),(5,'.')],[(3,'.'),(2,'#'),(4,'.'),(1,'#'),
(4,'.')],[(3,'.'),(2,'#'),(4,'.'),(1,'#'),(4,'.')],
[(4,'.'),(5,'#'),(5,'.')],[(14,'.')]]
ghci> printPicture pic
..............
.....##.......
....#.........
.....#........
.....##.......
....##.##.....
...##....#....
...##....#....
....#####.....
..............
ghci> printPicture $ flipV arrow `beside` invertColor arrow
..........##............
........#..#.##.........
......#....#.####.......
....#......#.######.....
..#........#.########...
#..........#.##########.
..#........#.########...
....#......#.######.....
......#....#.####.......
........#..#.##.........
..........##............
ghci> bes = flipV lambda `beside` invertColor lambda
ghci> ab = invertColor bes `above` flipH bes
ghci> printPicture ab
##############..............
##########..##..##..........
#########.####....#.........
########.#####.....#........
#######..#####.....##.......
######..#.####....#.##......
####....##.###...#..####....
##.....###..##..##...#####..
##############..............
..............##############
..#####...##..##..###.....##
....####..#...###.##....####
......##.#....####.#..######
.......##.....#####..#######
........#.....#####.########
.........#....####.#########
..........##..##..##########
..............##############
ghci> printPicture $ scale pic 2
printPicture $ scale pic 2
............................
............................
..........####..............
..........####..............
........##..................
........##..................
..........##................
..........##................
..........####..............
..........####..............
........####..####..........
........####..####..........
......####........##........
......####........##........
......####........##........
......####........##........
........##########..........
........##########..........
............................
............................
ghci> printPicture $ arrow `beside` invertColor lambda
##..........##############
#..#........##..##########
#....#......####.#########
#......#....#####.########
#........#..#####..#######
#..........#####.#..######
#........#..###.##....####
#......#....##..###.....##
#....#......##############
#..#......................
##........................

-}





