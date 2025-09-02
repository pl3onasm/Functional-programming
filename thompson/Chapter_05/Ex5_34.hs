import Chapter_05.Ex5_28
import Data.List (nub, intersperse)

-----------------------------------------------------------
-- Exercise 5.34

-- | Returns a string representation of the database
showDb :: Database -> String
showDb [] = "The database is empty."
showDb db = concat [pers ++ " has borrowed: " ++ 
              showBooks (books db pers) ++ "\n" 
              | pers <- nub [person | (person, _) <- db]]
   
-- | Returns a string representation of the books a 
-- person has borrowed
showBooksOf :: Database -> Person -> String
showBooksOf db pers =
  pers ++ " has borrowed: " ++ showBooks (books db pers)
  
-- | Returns a string representation of a list of books
showBooks :: [Book] -> String
showBooks [] = "no books"
showBooks bs = concat (intersperse ", " bs)

-- | Returns a string representation of the 
-- borrowers of a book
showBorrowersOf :: Database -> Book -> String
showBorrowersOf db bk 
  | null bs = "No one has borrowed" ++ bk ++ "."
  | otherwise = bk ++ " has been borrowed by: " 
                ++ concat (intersperse ", " bs) ++ "."
  where bs = borrowers db bk

-- | Returns a string representation of the number of
-- books a person has borrowed
showNumBorrowed :: Database -> Person -> String
showNumBorrowed db pers 
  | n == 0 = pers ++ " has not borrowed any books."
  | n == 1 = pers ++ " has borrowed 1 book."
  | otherwise = pers ++ " has borrowed " 
                ++ show n ++ " books."
  where n = numBorrowed db pers


-----------------------------------------------------------

{-

We used nub and intersperse from the Data.List module. 
The function nub removes duplicates from a list, while 
intersperse inserts a given element between the elements of 
a list: in this case, we used it to insert commas between
the book titles.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex5_34
ghci> putStr $ showDb exampleDb
Alice has borrowed: Tintin, Asterix
Anna has borrowed: Little Women
Rory has borrowed: Tintin
ghci> putStrLn $ showBooksOf exampleDb "Alice"
Alice has borrowed: Tintin, Asterix
ghci> putStrLn $ showBooksOf exampleDb "Bob"
Bob has borrowed: no books
ghci> putStrLn $ showBorrowersOf exampleDb "Tintin"
Tintin has been borrowed by: Alice, Rory.
ghci> putStrLn $ showBorrowersOf exampleDb "Demian"
No one has borrowed Demian.
ghci> putStrLn $ showNumBorrowed exampleDb "Alice"
Alice has borrowed 2 books.
ghci> putStrLn $ showNumBorrowed exampleDb "Anna"
Anna has borrowed 1 book.
ghci> putStrLn $ showNumBorrowed exampleDb "Bob"
Bob has not borrowed any books.

-}
