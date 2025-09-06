-----------------------------------------------------------
-- Exercise 6.27

-- | A picture is given by its width, the starting 
-- character ('.' or '#') and a list of runs which
-- alternate between the starting character and its
-- opposite.
type Picture = (Int, Char, [Int])

-- | Encodes a list of strings into a picture 
toPicture :: [String] -> Picture
toPicture rows =
  let flat = concat rows
  in (length (head rows), head (head rows), rle flat)

-- | Expands a picture into a list of strings (one per row).
expand :: Picture -> [String]
expand (width, start, runs) = split width (exp start runs)
  where
    exp _ []       = []
    exp c (n : ns) = replicate n c ++ exp (swap c) ns
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
invertColor (w, s, r) = (w, swap s, r)

-- | Encodes a list of strings into a picture
rle :: String -> [Int]
rle [] = error "Cannot RLE empty string"
rle (x : xs) = encode 1 x xs
  where
    encode n _ [] = [n]
    encode n c (y : ys)
      | y == c    = encode (n+1) c ys
      | otherwise = n : encode 1 y ys

-- | Stacks two pictures vertically, assuming same width
above :: Picture -> Picture -> Picture
above pic1 (_, _, []) = pic1  
above (_, _, []) pic2 = pic2
above (w1, s1, r1) (w2, s2, r2)
  | w1 /= w2 = error "Pictures must have the same width"
  | endChar1 /= s2 = (w1, s1, r1 ++ r2)
  | otherwise      = (w1, s1, mergedRuns)
  where
    endChar1 = if even (length r1) then swap s1 else s1
    mergedRuns = init r1 ++ [last r1 + head r2] ++ tail r2

-- | Places two pictures beside each other, assuming same height.
beside :: Picture -> Picture -> Picture
beside (_, _, []) pic2 = pic2
beside pic1 (_, _, []) = pic1
beside pic1 pic2
  | length rows1 /= length rows2 =
      error "Pictures must have the same height"
  | otherwise = (width, start, ns)
  where
    rows1 = expand pic1
    rows2 = expand pic2
    besideRows = zipWith (++) rows1 rows2
    width = length (head besideRows)
    start = head (head besideRows)
    ns = rle (concat besideRows)

-- | Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV (w, s, r) = 
  (w, s', rle $ concat [reverse row | row <- rows])
    where 
      rows = expand (w, s, r)
      s'   = last (head rows)

-- | Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH (w, s, r) = (w, s', rle $ concat [row | row <- rows])
  where 
    rows = reverse (expand (w, s, r))
    s'   = head (head rows)

-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Example picture
pic1 :: Picture
pic1 = (4, '.', [5,2,2,2,5])

-- | Example picture
pic2 :: Picture
pic2 = (4, '#', [5,2,2,2,5])

-- | Example picture
lambda :: Picture
lambda = (14, '.', [16,2,14,1,14,1,13,2,11,
                    1,1,2,9,1,2,4,6,2,3,5,16])

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
ghci> printPicture $ flipV lambda
..............
..........##..
.........#....
........#.....
.......##.....
......##.#....
....####..#...
..#####...##..
..............
ghci> printPicture $ flipH lambda
..............
..##...#####..
...#..####....
....#.##......
.....##.......
.....#........
....#.........
..##..........
..............
ghci> printPicture $ rotate lambda
..............
..#####...##..
....####..#...
......##.#....
.......##.....
........#.....
.........#....
..........##..
..............
ghci> printPicture $ lambda `beside` lambda
............................
..##............##..........
....#.............#.........
.....#.............#........
.....##............##.......
....#.##..........#.##......
...#..####.......#..####....
..##...#####....##...#####..
............................
ghci> printPicture $ lambda `above` lambda
..............
..##..........
....#.........
.....#........
.....##.......
....#.##......
...#..####....
..##...#####..
..............
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
(14,'.',[19,2,11,1,14,1,13,2,11,2,
        1,2,8,2,4,1,7,2,4,1,8,5,19])
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
ghci> ab = invertColor pic `above` lambda
ghci> bes = flipH ab `beside` ab
ghci> printPicture bes
..............##############
..##...#####..#####..#######
...#..####....####.#########
....#.##......#####.########
.....##.......#####..#######
.....#........####..#..#####
....#.........###..####.####
..##..........###..####.####
..............####.....#####
############################
####.....#####..............
###..####.####..##..........
###..####.####....#.........
####..#..#####.....#........
#####..#######.....##.......
#####.########....#.##......
####.#########...#..####....
#####..#######..##...#####..
##############..............

-}