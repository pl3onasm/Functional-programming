module Chapter_06.Ex6_37 where

import Chapter_06.Ex6_35
import Chapter_06.Ex6_36
import Chapter_06.Ex6_32 (printIm)
import Chapter_06.Ex6_15 (invertColor)
import Chapter_06.Ex6_34 (flipImH, flipImV)

-----------------------------------------------------------
-- Exercise 6.37

-- | Stacks two images vertically: the first image on top
-- of the second one
above :: Image -> Image -> Image
above (pic1, _) (pic2, (x2, y2)) =
  superImpose (pic1, (x2, y2 + h2)) (pic2, (x2, y2))
  where
    h2 = length pic2

-- | Puts two images beside each other: the second image
-- to the right of the first one
beside :: Image -> Image -> Image
beside (pic1, (x1, y1)) (pic2, _) =
  superImpose (pic1, (x1, y1)) (pic2,(x1 + w1, y1))
  where
    w1 = length (head pic1)


-----------------------------------------------------------

{-

To implement the analogues of the above and beside 
functions for images, we can use the superImpose function
from Exercise 6.36. In order to put one image above 
another, we just need to adjust the position of the first
image so that its reference point is above the reference
point of the second image by the height of the second
image. Similarly, to put one image beside another, we
need to adjust the position of the second image so that
its reference point is to the right of the reference
point of the first image by the width of the first image.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_37
ghci> ab = delta `above` invertIm lambda
ghci> printIm ab
..............
.....##.......
....#.........
.....##.......
.....###......
...##....#....
..##......#...
..##.....##...
...######.....
..............
##############
###..#########
#####.########
######.#######
######..######
#####.#..#####
####.###..####
###..####..###
##...###....##
##############
ghci> bes = flipImH ab `beside` flipImV ab
ghci> printIm bes
##############..............
##...###....##.......##.....
###..####..###.........#....
####.###..####.......##.....
#####.#..#####......###.....
######..######....#....##...
######.#######...#......##..
#####.########...##.....##..
###..#########.....######...
##############..............
..............##############
...######.....#########..###
..##.....##...########.#####
..##......#...#######.######
...##....#....######..######
.....###......#####..#.#####
.....##.......####..###.####
....#.........###..####..###
.....##.......##....###...##
..............##############


-}