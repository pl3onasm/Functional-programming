-----------------------------------------------------------
-- Exercise 19.1

-- implements a simple word, line, and character counter
-- reads from standard input and outputs the counts
main :: IO ()
main = do
  input <- getContents
  let lineCount = length (lines input)
      wordCount = length (words input)
      charCount = length input
  putStrLn (show lineCount ++ " " ++ 
            show wordCount ++ " " ++ 
            show charCount)


{- There are two ways to run this program:

1. Compile it with GHC and run the resulting executable, 
   providing input through standard input:

   $ ghc ex19-1.hs -o wc
   $ ./wc < test1.txt

2. Run it using runghc, which compiles and runs the 
   program in one step:

   $ runghc ex19-1.hs < test1.txt
-}