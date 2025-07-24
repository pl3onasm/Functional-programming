import Prelude hiding (replicate)

-----------------------------------------------------------
-- Exercise 5.4

-- implements replicate using a list comprehension;
-- note that the generator only serves as a counter
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-----------------------------------------------------------

{- Examples:

  ghci> :l ex5-4.hs
  ghci> replicate 15 'a'
  "aaaaaaaaaaaaaaa"
  ghci> replicate 6 3
  [3,3,3,3,3,3]

-}