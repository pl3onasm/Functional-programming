-----------------------------------------------------------
-- Exercise 10.5

-- prompts the user for a number of integers, 
-- reads them from the input, and prints their total
adder :: IO ()
adder = do
  putStr "How many numbers? "
  x <- readNumber
  total <- sequence (replicate x readNumber)
  putStrLn $ "The total is: " ++ show (sum total)
  where
    readNumber :: IO Int
    readNumber = do
      input <- getLine
      return (read input :: Int)

-----------------------------------------------------------