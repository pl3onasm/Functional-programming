import Library.Pictures

-----------------------------------------------------------
-- Exercise 4.5

-- | This method first swaps the pics on the left 
-- vertically by inverting the colours, then flips that 
-- result in a vertical mirror
fourPics :: Picture -> Picture
fourPics pic = left `beside` right
  where
    left  = pic `above` invertColour pic
    right = flipV (invertColour left)

-- | This uses a helper function to stack the pics 
-- vertically. It gives the left half the normal pic to 
-- stack, and the right half the inverted pic to stack. 
-- Finally, it flips the right half in a vertical mirror.
fourPics' :: Picture -> Picture
fourPics' pic =
  left `beside` right
  where
    stack p  = p `above` invertColour p
    left  = stack pic
    right = flipV (stack (invertColour pic))
    

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex4_05
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