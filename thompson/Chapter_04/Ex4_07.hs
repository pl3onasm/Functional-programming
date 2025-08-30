import Library.Pictures

-----------------------------------------------------------
-- Exercise 4.7

-- | This method first creates the top half. Then it 
-- creates the bottom half by flipping the top half
-- in a horizontal mirror and rotating it.
fourPics :: Picture -> Picture
fourPics pic = top `above` bottom
  where
    top    = pic `beside` flipV (invertColour pic)
    bottom = rotate (flipH top)

-- | Here we create the left half first. Then we create
-- the right half by flipping the left half in a horizontal
-- mirror and rotating it. Finally, we invert the colours 
-- of the right half. 
fourPics' :: Picture -> Picture
fourPics' pic = left `beside` right
  where
    left  = pic `above` invertColour pic
    right = invertColour (rotate (flipH left))
    

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex4_07
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