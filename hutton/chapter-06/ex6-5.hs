import Prelude hiding (length, drop, init)

-----------------------------------------------------------
-- Exercise 6.5

-- takes a list and computes its length
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

-- takes an integer n and a list, and removes the first n
-- elements from the input list
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs


-- takes a list as input, and removes its last element
init :: [a] -> [a]
init [_] = []
init (x : xs) = x : init xs

-----------------------------------------------------------

{-
  Evaluation of length [1,2,3]

    length [1,2,3]
  =     { applying length }
    1 + length [1,2]
  =     { applying length }
    1 + (1 + length [1])
  =     { applying length }
    1 + (1 + (1 + length []))
  =     { applying length }
    1 + (1 + (1 + 0)
  =     { applying + }
    3

  Evaluation of drop 3 [1,2,3,4,5]

    drop 3 [1,2,3,4,5]
  =     { applying drop }
    drop 2 [2,3,4,5]
  =     { applying drop }
    drop 1 [3,4,5]
  =     { applying drop }
    drop 0 [4,5]
  =     { applying drop }
    [4,5]

  Evaluation of init [1,2,3]

    init [1,2,3]
  =     { applying init }
    1 : init [2,3]
  =     { applying init }
    1 : 2 : init [3]
  =     { applying init }
    1 : 2 : []
  =     { list notation }
    [1,2]

-}