import Library.Pictures
import Chapter_04.Ex4_27
import Chapter_04.Ex4_28


-----------------------------------------------------------
-- Exercise 4.29

-- | Builds a white n x n square with black diagonals from 
-- top-left to bottom-right and top-right to bottom-left
diagonals :: Integer -> Picture
diagonals n
  | n <= 0    = error "diagonals: non-positive size"
  | otherwise = rows 0
  where
    rows r
      | r == n - 1 = cols r 0          -- last row
      | otherwise  = cols r 0 `above` rows (r + 1)
    cols r c
      | c == n - 1 = if diag then black else white
      | diag       = black `beside` cols r (c + 1)
      | otherwise  = white `beside` cols r (c + 1)
      where diag = r == c || r + c == n - 1

-- | Alternative solution using earlier defined functions
-- and superimpose from Library.Pictures
diagonals' :: Integer -> Picture
diagonals' n = superimpose (diagonal n) (antiDiagonal n)


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :load Ex4_29
ghci> printPicture (diagonals 5)
######..................######
######..................######
######..................######
######..................######
######..................######
######..................######
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......
............######............
............######............
............######............
............######............
............######............
............######............
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......
......######......######......
######..................######
######..................######
######..................######
######..................######
######..................######
######..................######


-}