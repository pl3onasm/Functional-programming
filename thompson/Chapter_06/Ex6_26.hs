type Picture = (Int, [(Int, Char)])

-----------------------------------------------------------
-- Exercise 6.26

-- | Expands a picture into a list of strings (one per row)
expand :: Picture -> [String]
expand (width, runs) = split width $ expandRuns runs
  where
    expandRuns rs = [c | (n,c) <- rs, _ <- [1..n]]
    split w s = [take w (drop (i*w) s) 
                 | i <- [0 .. (length s `div` w) - 1]]

-- | Prints a picture to the console.
printPicture :: Picture -> IO ()
printPicture pic = putStr $ unlines (expand pic)

-- | Stacks two pictures vertically, assuming same width
above :: Picture -> Picture -> Picture
above (w1, r1) (w2, r2)
  | w1 /= w2  = error "Pictures must have the same width"
  | otherwise = (w1, r1 ++ r2)

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
beside (w1, r1) (w2, r2)
  | length rows1 /= length rows2 = 
      error "Pictures must have the same height"
  | otherwise = 
      (w1 + w2, flatten (zipWith rowBeside rows1 rows2))
  where
    rows1 = expand (w1, r1)
    rows2 = expand (w2, r2)
    rowBeside s1 s2 = rle (s1 ++ s2)
    flatten []         = []
    flatten (r : rest) = r ++ flatten rest

-- Flips a picture in a vertical mirror
flipV :: Picture -> Picture
flipV (w, runs) =
  (w, concat [rle (reverse row) | row <- expand (w, runs)])

-- Flips a picture in a horizontal mirror
flipH :: Picture -> Picture
flipH (w, runs) =
  (w, concat [rle row | row <- reverse (expand (w, runs))])
    
-- Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Example picture
pic :: Picture
pic = (4, [(1,'.'),(2,'#'),(2,'.'),
           (1,'#'),(1,'.'),(1,'#'),
           (1,'.'),(7,'#')])

-- | Example picture
pic2 :: Picture
pic2 = (4, [(4,'#'),(1,'#'),
            (2,'.'),(2,'#'),
            (2,'.'),(5,'#')])


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex6_26
ghci> printPicture pic
.##.
.#.#
.###
####
ghci> printPicture $ flipV pic
.##.
#.#.
###.
####
ghci> printPicture $ flipH pic
####
.###
.#.#
.##.
ghci> printPicture $ rotate pic
####
###.
#.#.
.##.
ghci> printPicture $ pic 2
####
#..#
#..#
####
ghci> printPicture $ pic2 `beside` pic
####.##.
#..#.#.#
#..#.###
########
ghci> printPicture $ pic2 `above` pic
####
#..#
#..#
####
.##.
.#.#
.###
####

-}