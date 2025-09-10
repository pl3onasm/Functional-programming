import qualified Data.Set as S

-----------------------------------------------------------
-- Exercise 6.28, part 1

-- | A picture is represented as a triple consisting of its
-- width, height, and a set of points  where each point is 
-- a pair (x,y) of coordinates of a black pixel. 
-- The background is assumed to be white.
type Point = (Int, Int)
type Width = Int
type Height = Int
type Picture = (Width, Height, S.Set Point)

-- | Expands a picture into a list of strings (one per row)
expand :: Picture -> [String]
expand (w, h, pts) =
  [[if (x,y) `S.member` pts then '#' else '.' 
    | x <- [0..w-1]] | y <- [0..h-1]]

-- | Prints a picture
printPicture :: Picture -> IO ()
printPicture = putStr . unlines . expand

-- | Inverts the color of a picture by swapping
-- the set of black points with its complement
invertColor :: Picture -> Picture
invertColor (w, h, pts) = (w, h, allPts S.\\ pts)
  where
  allPts = S.fromList [(x,y) | x <-[0..w-1], y <-[0..h-1]]

-- | Flips a picture in a vertical mirror by mapping
-- each point (c,r) to (w-1-c,r)
flipV :: Picture -> Picture
flipV (w, h, pts) =
  (w, h, S.map (\(x,y) -> (w-x-1, y)) pts)

-- | Flips a picture in a horizontal mirror by mapping
-- each point (c,r) to (c,h-1-r)
flipH :: Picture -> Picture
flipH (w, h, pts) =
  (w, h, S.map (\(x,y) -> (x, h-y-1)) pts)

-- | Rotates a picture 180 degrees
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | Translates a set of points by (dx, dy)
movePts :: S.Set Point -> (Int, Int) -> S.Set Point
movePts pts (dx, dy) = S.map (\(x,y) -> (x+dx, y+dy)) pts

-- | Stacks two pictures of equal width by translating
-- the second picture down by the height of the first 
above :: Picture -> Picture -> Picture
above (w1, h1, pts1) (w2, h2, pts2)
  | w1 /= w2  = error "above: widths must match"
  | otherwise =
      (w1, h1+h2, pts1 `S.union` movePts pts2 (0, h1))

-- | Places two pictures of equal height beside each other
-- by translating the second picture right by the width
-- of the first
beside :: Picture -> Picture -> Picture
beside (w1, h1, pts1) (w2, h2, pts2)
  | h1 /= h2  = error "beside: heights must match"
  | otherwise =
      (w1+w2, h1, pts1 `S.union` movePts pts2 (w1, 0))

-- | Constructs a picture from a list of strings 
toPicture :: [String] -> Picture
toPicture [] = error "toPicture: empty list"
toPicture rows =  (w, h, S.fromList pts)
  where 
    h = length rows
    w = length (head rows)
    pts = [(x,y) | (y,row) <- zip [0..] rows,
                   (x,ch)  <- zip [0..] row, ch == '#']

-- | Example picture: a delta shape
delta :: Picture
delta = (14,10,S.fromList [(3,6),(3,7),(4,2),(4,5),(4,6),
                           (4,7),(4,8),(5,1),(5,3),(5,4),
                           (5,5),(5,8),(6,1),(6,4),(6,8),
                           (7,5),(7,8),(8,5),(8,8),(9,6),
                           (9,7)])

-- | List of strings: a lambda shape
lambda :: [String]
lambda =
  ["..............",
   "..##..........",
   "....#.........",
   ".....#........",
   ".....##.......",
   "....#.##......",
   "...#..####....",
   "..##...#####..",
   ".............."]


-----------------------------------------------------------

{-

In this first part of the exercise we define Picture to be 
a triple consisting of its width, height, and a set of 
points representing the black pixels only. The background 
is assumed to be white.

For this we use the Data.Set module from the containers
package. We need to import it qualified to avoid name
clashes with Prelude functions. Using sets is more
efficient than using lists, especially for large pictures.
An example is the member function, which checks whether a
given point is in the set. This is used when expanding
the picture into a list of strings for printing. The
complexity of member is O(log n) for sets, compared to
O(n) for lists.

Documentation for this package can be found at
  https://hackage.haskell.org/package/containers
under the section "Data.Set".

Testing in GHCi

ghci> :set -package containers
ghci> :load Ex6_28
ghci> printPicture delta
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
ghci> pic = toPicture lambda
ghci> pic
(14,9,fromList [(2,1),(2,7),(3,1),(3,6),(3,7),(4,2),
(4,5),(5,3),(5,4),(6,4),(6,5),(6,6),(7,5),(7,6),(7,7),
(8,6),(8,7),(9,6),(9,7),(10,7),(11,7)])
ghci> printPicture $ invertColor pic
##############
##..##########
####.#########
#####.########
#####..#######
####.#..######
###.##....####
##..###.....##
##############
ghci> ab = pic `above ` flipH delta
ghci> printPicture ab
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
....#####.....
...##....#....
...##....#....
....##.##.....
.....##.......
.....#........
....#.........
.....##.......
..............
ghci> bes = flipV ab `beside` invertColor ab
ghci> printPicture bes
..............##############
..........##..##..##########
.........#....####.#########
........#.....#####.########
.......##.....#####..#######
......##.#....####.#..######
....####..#...###.##....####
..#####...##..##..###.....##
..............##############
..............##############
.....#####....####.....#####
....#....##...###..####.####
....#....##...###..####.####
.....##.##....####..#..#####
.......##.....#####..#######
........#.....#####.########
.........#....####.#########
.......##.....#####..#######
..............##############
ghci> printPicture $ rotate bes
##############..............
#######..#####.....##.......
#########.####....#.........
########.#####.....#........
#######..#####.....##.......
#####..#..####....##.##.....
####.####..###...##....#....
####.####..###...##....#....
#####.....####....#####.....
##############..............
##############..............
##.....###..##..##...#####..
####....##.###...#..####....
######..#.####....#.##......
#######..#####.....##.......
########.#####.....#........
#########.####....#.........
##########..##..##..........
##############..............

-}