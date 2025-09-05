import Chapter_06.Ex6_08 (padToRect)

import Library.Pictures

-----------------------------------------------------------
-- Exercise 6.17

-- | Stacks two pictures, one above the other
above :: Picture -> Picture -> Picture
above = (++)

-- | Places two pictures beside each other
beside :: Picture -> Picture -> Picture
beside picL picR
    = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

-- | Modified above guaranteeing rectangular result
abv :: Picture -> Picture -> Picture
abv picT picB = padToRect $ above picT picB

-- | Modified beside guaranteeing rectangular result
besd :: Picture -> Picture -> Picture
besd picL picR = padToRect $ beside picL picR


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_16
ghci> pic = (horse `besd` horse) `abv` horse
ghci> printPicture pic
.......##..........##...
.....##..#.......##..#..
...##.....#....##.....#.
..#.......#...#.......#.
..#...#...#...#...#...#.
..#...###.#...#...###.#.
.#....#..##..#....#..##.
..#...#.......#...#.....
...#...#.......#...#....
....#..#........#..#....
.....#.#.........#.#....
......##..........##....
.......##...............
.....##..#..............
...##.....#.............
..#.......#.............
..#...#...#.............
..#...###.#.............
.#....#..##.............
..#...#.................
...#...#................
....#..#................
.....#.#................
......##................

-}
