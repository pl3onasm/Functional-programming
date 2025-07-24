-----------------------------------------------------------
-- Exercise 10.3

type Board = [Int]

-- prints a row of the board
-- rowNr is the row number, num is the number of stars
-- the row numbers start at 1
putRow :: Int -> Int -> IO ()
putRow rowNr num = do 
                   putStr (show rowNr)
                   putStr ": "
                   putStrLn (concat (replicate num "* "))

-- prints the board in linear time using a list
-- comprehension and sequence_ :: [IO a] -> IO ()
putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow i x | (i,x) <- zip [1..] b]

-----------------------------------------------------------