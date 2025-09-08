-----------------------------------------------------------
-- Exercise 6.28, part 2

-- | A picture is represented as a function from (x,y)
-- coordinates to a Bool value indicating whether the pixel
-- at that position is set ('#') or clear ('.').
-- The first two Int values represent the width and height
-- of the picture.
type Point   = (Int, Int)
type Width   = Int
type Height  = Int
type Picture = (Width, Height, Point -> Bool)

-- | Turns a list of strings into a picture
toPicture :: [String] -> Picture
toPicture [] = error "toPicture: empty list"
toPicture rows = (width, height, f)
  where
    height = length rows
    width  = length (head rows)
    f (x, y)
      | x < 0 || y < 0 || x >= width || y >= height = False
      | otherwise = (rows !! y) !! x == '#'

-- | Expands a picture into a list of strings (one per row)
expand :: Picture -> [String]
expand (width, height, f) =
  [[if f (x, y) then '#' else '.' | x <- [0..width-1]] 
                                  | y <- [0..height-1]]

-- | Prints a picture to the console
printPicture :: Picture -> IO ()
printPicture = putStr . unlines . expand 

-- | Inverts the color of a picture by flipping the Bool
-- value returned by the function
invertColor :: Picture -> Picture
invertColor (w, h, f) = (w, h, \p -> not (f p))

-- | Stacks two pictures of equal width vertically by
-- adjusting the function of the 2nd picture to accept 
-- y-coords shifted down by the height of the 1st picture
above :: Picture -> Picture -> Picture
above (w1, h1, f1) (w2, h2, f2)
  | w1 /= w2  = error "above: widths must match"
  | otherwise = (w1, h1+h2, \(x,y) ->
      if y < h1 then f1 (x,y) else f2 (x, y - h1))

-- | Places two pictures of equal height beside each other
-- by adjusting the function of the 2nd picture to accept
-- x-coords shifted right by the width of the 1st picture
beside :: Picture -> Picture -> Picture
beside (w1, h1, f1) (w2, h2, f2)
  | h1 /= h2 = error "beside: heights must match"
  | otherwise = (w1+w2, h1, \(x,y) ->
      if x < w1 then f1 (x,y) else f2 (x - w1, y))

-- | Flips a picture in a vertical mirror by adjusting the
-- function to map x-coordinates to their mirrored position
flipV :: Picture -> Picture
flipV (w, h, f) = (w, h, \(x,y) -> f (w - x - 1, y))

-- | Flips a picture in a horizontal mirror by adjusting the
-- function to map y-coordinates to their mirrored position
flipH :: Picture -> Picture
flipH (w, h, f) = (w, h, \(x,y) -> f (x, h - y - 1))

-- | Rotates a picture 180 degrees by adjusting the function
-- to map (x,y) to their rotated position
rotate :: Picture -> Picture
rotate = flipH . flipV

-- | List of strings: a delta shape
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

In this second part of the exercise we define Picture to 
be a triple consisting of its width, height, and a function
that maps (x,y) coordinates to a Bool value indicating 
whether the pixel at that position is black ('#') or not.
The background is assumed to be white ('.').

If the function is computationally heavy, then the code can 
be optimized by memoizing the function, so that it does not 
have to be recomputed for every pixel when printing the 
picture. This can be done by using the vector library, or a 
map keyed on Point. There is also a library called 
Data.MemoCombinators that can be used for memoization. 
This way, calls to the function for the same (x,y) 
coordinates will return the cached result in constant time, 
instead of recomputing it each time.

Using this representation, we can also define infitely 
large pictures, such as tiling patterns. However, the
printPicture function will only print the part that fits
within the specified width and height.

Testing in GHCi

ghci> :load Ex6_28b
ghci> del = toPicture delta
ghci> lam = toPicture lambda
ghci> printPicture del
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
ghci> bes = flipV lam `beside` flipH lam
ghci> printPicture bes  
............................
..........##....##...#####..
.........#.......#..####....
........#.........#.##......
.......##..........##.......
......##.#.........#........
....####..#.......#.........
..#####...##....##..........
............................
ghci> ab = flipH bes `above` invertColor bes
ghci> printPicture ab
............................
..#####...##....##..........
....####..#.......#.........
......##.#.........#........
.......##..........##.......
........#.........#.##......
.........#.......#..####....
..........##....##...#####..
............................
############################
##########..####..###.....##
#########.#######.##....####
########.#########.#..######
#######..##########..#######
######..#.#########.########
####....##.#######.#########
##.....###..####..##########
############################
ghci> printPicture $ rotate ab
############################
##########..####..###.....##
#########.#######.##....####
########.#########.#..######
#######..##########..#######
######..#.#########.########
####....##.#######.#########
##.....###..####..##########
############################
............................
..#####...##....##..........
....####..#.......#.........
......##.#.........#........
.......##..........##.......
........#.........#.##......
.........#.......#..####....
..........##....##...#####..
............................

-}