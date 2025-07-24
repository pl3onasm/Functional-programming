import Prelude hiding (putStr)

-----------------------------------------------------------
-- Exercise 10.1

-- prints a string on the standard output
-- definiton given in the book
putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do putChar x
                     putStr xs 

-- redefines putStr using a list comprehension 
-- and the library function sequence_
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-----------------------------------------------------------

{-
  The first version of the function recursively processes
  the string one character at a time, printing each
  character using the putChar function.

  The second version uses a list comprehension to create
  a list of IO () actions, one for each character, and
  then applies the sequence_ function to execute them all
  in order. The sequence_ function discards the results
  of the actions, which are all () in this case.
-}