-----------------------------------------------------------
-- Exercise 10.2

type Board = [Int]

-- prints a row of the board
-- rowNr is the row number, num is the number of stars
-- the row numbers start at 1
putRow :: Int -> Int -> IO ()
putRow rowNr num = do 
                   putStr (show rowNr)
                   putStr ": "
                   putStrLn (concat (replicate num "* "))

-- prints the board row by row
-- works in linear time
putBoard :: Board -> IO ()
putBoard b = ptbrd b 1
  where 
    ptbrd []        _ = return ()
    ptbrd (x : xs)  i = do putRow i x
                           ptbrd xs (i + 1)  

-- same but using zip
putBoard' :: Board -> IO ()
putBoard' b = pbd (zip [1..] b)
  where
    pbd []            = return ()
    pbd ((i, x) : xs) = do putRow i x
                           pbd xs

-----------------------------------------------------------

{-
  Example:

  ghci> :l ex10-2
  ghci> putBoard [2,6,4,5,1,8]
  1: * *
  2: * * * * * *
  3: * * * *
  4: * * * * *
  5: *
  6: * * * * * * * *

-}