module Chapter_04.Ex4_25 where

import Library.Pictures

-----------------------------------------------------------
-- Exercise 4.25

-- | Builds an alternating sequence of black and white 
-- squares, starting with a black square at the left
bwRow :: Integer -> Picture
bwRow n
  | n <= 1    = black
  | otherwise = black `beside` wbRow (n - 1)

-- | Builds an alternating sequence of black and white 
-- squares, starting with a white square at the left
wbRow :: Integer -> Picture
wbRow n
  | n <= 1    = white
  | otherwise = white `beside` bwRow (n - 1)

-- | Builds an n x m chessboard pattern, starting with a
-- black square in the top-left corner
blackChess :: Integer -> Integer -> Picture
blackChess n m
  | n <= 1    = bwRow m
  | otherwise = bwRow m `above` whiteChess (n - 1) m

-- | Builds an n x m chessboard pattern, starting with a
-- white square in the top-left corner
whiteChess :: Integer -> Integer -> Picture
whiteChess n m
  | n <= 1    = wbRow m
  | otherwise = wbRow m `above` blackChess (n - 1) m


-----------------------------------------------------------

{-

Testing in GHCi:

ghci> :set -i..
ghci> :load Ex4_25.hs

ghci>printPicture (whiteBlack 5)
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......

ghci> printPicture (whiteChess 3 7)
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......
######......######......######......######
######......######......######......######
######......######......######......######
######......######......######......######
######......######......######......######
######......######......######......######
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......
......######......######......######......


-}