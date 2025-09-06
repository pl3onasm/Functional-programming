-----------------------------------------------------------
-- Exercise 6.26

-- A picture is represented as a tuple consisting of its
-- width, and a list of (count, char) pairs representing
-- the run-length encoding of the picture
type Picture = (Int, [(Int, Char)])

-- | Encodes a list of strings into a picture
toPicture :: [String] -> Picture
toPicture rows =
  let flat = concat rows
  in (length (head rows), rle flat)

-- | Expands a picture into a list of strings (one per row)
expand :: Picture -> [String]
expand (width, runs) = split width $ expRuns runs
  where
    expRuns rs = [c | (n,c) <- rs, _ <- [1..n]]
    split w [] = []
    split w s  = take w s : split w (drop w s)

-- | Prints a picture to the console.
printPicture :: Picture -> IO ()
printPicture pic = putStr $ unlines (expand pic)

-- | Swap '.' and '#'.
swap :: Char -> Char
swap '.' = '#'
swap '#' = '.'
swap  c  = error $ "Invalid char in picture: " ++ [c]

-- | Inverts the color of a picture
invertColor :: Picture -> Picture
invertColor (w, rs) = (w, [(n, swap c) | (n,c) <- rs])

-- | Stacks two pictures vertically, assuming same width
above :: Picture -> Picture -> Picture
above (_, []) pic2 = pic2
above pic1 (_, []) = pic1
above (w1, r1) (w2, r2)
  | w1 /= w2  = error "Pictures must have the same width"
  | endCh1 /= startCh2 = (w1, r1 ++ r2)
  | otherwise = (w1, mergedRuns)
  where
    (n1, endCh1)   = last r1
    (n2, startCh2) = head r2
    mergedRuns = init r1 ++ [(n1 + n2, endCh1)] ++ tail r2

-- | Run-length encodes a string
rle :: String -> [(Int, Char)]
rle []       = []
rle (x : xs) = encode 1 x xs
  where
    encode n c [] = [(n,c)]
    encode n c (y : ys)
      | y == c    = encode (n+1) c ys
      | otherwise = (n,c) : encode 1 y ys

-- | Places two pictures of equal height beside each other
beside :: Picture -> Picture -> Picture
beside (_, []) pic2 = pic2
beside pic1 (_, []) = pic1
beside (w1, r1) (w2, r2)
  | length rows1 /= length rows2 = 
      error "Pictures must have the same height"
  | otherwise = 
      (w1 + w2, rs) 
  where
    rows1 = expand (w1, r1)
    rows2 = expand (w2, r2)
    besideRows = zipWith (++) rows1 rows2
    rs = rle (concat besideRows)
    
-- | Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV (w, runs) =
  (w, concat [rle (reverse row) | row <- expand (w, runs)])

-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH (w, runs) =
  (w, concat [rle row | row <- reverse (expand (w, runs))])
    
-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Example picture
lambda :: Picture
lambda = 
  (14,[(16,'.'),(2,'#'),(14,'.'),(1,'#'),
       (14,'.'),(1,'#'),(13,'.'),(2,'#'),
       (11,'.'),(1,'#'),(1,'.'),(2,'#'),
       (9,'.'),(1,'#'),(2,'.'),(4,'#'),(6,'.'),
       (2,'#'),(3,'.'),(5,'#'),(16,'.')])

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


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_26
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
ghci> printPicture $ invertColor lambda
##############
##..##########
####.#########
#####.########
#####..#######
####.#..######
###.##....####
##..###.....##
##############
ghci> pic = toPicture delta
ghci> pic
(14,[(19,'.'),(2,'#'),(11,'.'),(1,'#'),(14,'.'),
(1,'#'),(13,'.'),(2,'#'),(11,'.'),(2,'#'),(1,'.'),
(2,'#'),(8,'.'),(2,'#'),(4,'.'),(1,'#'),(7,'.'),
(2,'#'),(4,'.'),(1,'#'),(8,'.'),(5,'#'),(19,'.')])
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
ghci> printPicture $ flipV pic
..............
.......##.....
.........#....
........#.....
.......##.....
.....##.##....
....#....##...
....#....##...
.....#####....
..............
ghci> printPicture $ flipH pic
..............
....#####.....
...##....#....
...##....#....
....##.##.....
.....##.......
.....#........
....#.........
.....##.......
..............
ghci> printPicture $ rotate pic
..............
.....#####....
....#....##...
....#....##...
.....##.##....
.......##.....
........#.....
.........#....
.......##.....
..............
ghci> ab = pic `above` invertColor lambda
ghci> printPicture $ ab
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
##############
##..##########
####.#########
#####.########
#####..#######
####.#..######
###.##....####
##..###.....##
##############
ghci> printPicture $ ab `beside` invertColor ab
..............##############
.....##.......#####..#######
....#.........####.#########
.....#........#####.########
.....##.......#####..#######
....##.##.....####..#..#####
...##....#....###..####.####
...##....#....###..####.####
....#####.....####.....#####
..............##############
##############..............
##..##########..##..........
####.#########....#.........
#####.########.....#........
#####..#######.....##.......
####.#..######....#.##......
###.##....####...#..####....
##..###.....##..##...#####..
##############..............


-}