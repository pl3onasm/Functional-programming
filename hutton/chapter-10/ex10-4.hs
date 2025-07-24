-----------------------------------------------------------
-- Exercise 10.4

-- prompts the user for a number of integers, 
-- reads them from the input, and prints their total
adder :: IO ()
adder = do putStr "How many numbers? "
           input <- getLine
           let x = read input :: Int
           total <- add x 0
           putStrLn $ "The total is: " ++ show total
  where
    add 0 total = return total
    add n total = 
      do input <- getLine
         let x = read input :: Int
         add (n - 1) (total + x)

-----------------------------------------------------------

{-
  Example:

  ghci> :l ex10-4
  ghci> adder
  How many numbers? 7
  1
  2
  3
  5
  8
  13
  21
  The total is: 57

-}