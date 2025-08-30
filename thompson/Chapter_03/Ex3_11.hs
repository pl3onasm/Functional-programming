module Chapter_03.Ex3_11 where

-----------------------------------------------------------
-- Exercise 3.11

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual x y z = (x == y) && (y == z)

mystery :: Integer -> Integer -> Integer -> Bool
mystery x y z = not ((x == y) && (y == z))

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)

fourEqual :: Integer -> Integer -> Integer -> Integer 
             -> Bool
fourEqual w x y z = (w == x) && (x == y) && (y == z)

-----------------------------------------------------------

{-

Calculating threeEqual (2+3) 5 (11 `div` 2):

  threeEqual (2+3) 5 (11 `div` 2)
=   { applying threeEqual }
  ((2+3) == 5) && (5 == (11 `div` 2))
=   { evaluating + and div }
  (5 == 5) && (5 == 5)
=   { evaluating the comparisons }
  True && True
=   { evaluating && }
  True

Calculating mystery (2+4) 5 (11 `div` 2):

  mystery (2+4) 5 (11 `div` 2)
=   { applying mystery }
  not (((2+4) == 5) && (5 == (11 `div` 2)))
=   { evaluating + and div }
  not ((6 == 5) && (5 == 5))
=   { evaluating the comparisons }
  not (False && True)
=   { evaluating && }
  not False
=   { evaluating not }
  True

Calculating threeDifferent (2+4) 5 (11 `div` 2):

  threeDifferent (2+4) 5 (11 `div` 2)
=   { applying threeDifferent }
  ((2+4) /= 5) && (5 /= (11 `div` 2)) 
  && ((2+4) /= (11 `div` 2))
=   { evaluating + and div }
  (6 /= 5) && (5 /= 5) && (6 /= 5)
=   { evaluating the comparisons }
  True && False && True
=   { evaluating the first && }
  False && True
=   { evaluating the remaining && }
  False

Calculating 
    fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11):

  fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11)
=   { applying fourEqual }
  ((2+3) == 5) && (5 == (11 `div` 2)) 
  && ((11 `div` 2) == (21 `mod` 11))
=   { evaluating +, div and mod }
  (5 == 5) && (5 == 5) && (5 == 10)
=   { evaluating the comparisons }
  True && True && False
=   { evaluating the first && }
  True && False
=   { evaluating the remaining && }
  False


-}