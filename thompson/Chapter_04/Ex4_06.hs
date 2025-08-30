import Library.Pictures

-----------------------------------------------------------
-- Exercise 4.6

-- | This method produces the four pictures independently.
fourPics :: Picture -> Picture
fourPics pic = top `above` bottom
  where
    top    = pic `beside` flipV (invertColour pic)
    bottom = invertColour pic `beside` flipV pic

-- | This method reuses the top half. It simply inverts
-- the colours of the top half to produce the bottom half.
fourPics' :: Picture -> Picture
fourPics' pic = top `above` bottom
  where
    top    = pic `beside` flipV (invertColour pic)
    bottom = invertColour top

-- | Here we use a helper function that stacks a picture
-- next to its flipped inverted version. The top half 
-- then gets the original picture to be stacked, while
-- the bottom half gets to stack the inverted picture.
fourPics'' :: Picture -> Picture
fourPics'' pic = top `above` bottom
  where
    stack p = p `beside` invertColour (flipV p)
    top      = stack pic
    bottom   = stack (invertColour pic)
    

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex4_06
ghci> printPicture (fourPics horse)
.......##...###..#######
.....##..#..##.##..#####
...##.....#.#.#####..###
..#.......#.#.#######.##
..#...#...#.#.###.###.##
..#...###.#.#.#...###.##
.#....#..##.#..##.####.#
..#...#.....#####.###.##
...#...#....####.###.###
....#..#....####.##.####
.....#.#....####.#.#####
......##....####..######
#######..###...##.......
#####..##.##..#..##.....
###..#####.#.#.....##...
##.#######.#.#.......#..
##.###.###.#.#...#...#..
##.###...#.#.#.###...#..
#.####.##..#.##..#....#.
##.###.#####.....#...#..
###.###.####....#...#...
####.##.####....#..#....
#####.#.####....#.#.....
######..####....##......


-}