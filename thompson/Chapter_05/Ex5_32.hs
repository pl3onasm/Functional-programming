module Chapter_05.Ex5_32 where

-----------------------------------------------------------
-- Exercise 5.32

type Person = String
type Book = String

type Database = [(Person , [Book])]

-- | Returns the books that a person has borrowed, if any
books :: Database -> Person -> [Book] 
books dBase findPerson = concat
  [books | (pers, books) <- dBase, pers == findPerson]

-- | Returns the borrowers for a given book, if any
borrowers :: Database -> Book -> [Person]
borrowers dBase findBook =
  [pers | (pers, books) <- dBase, findBook `elem` books]

-- | Returns if a book is borrowed or not
borrowed :: Database -> Book -> Bool
borrowed dBase findBook = [] /= borrowers dBase findBook

-- | Returns the number of books a person has borrowed
numBorrowed :: Database -> Person -> Int
numBorrowed dBase findPerson 
  = length (books dBase findPerson)

-- | Makes a loan of a book to a person and returns
-- the updated database
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase pers bk
  | null currentBooks = [(pers, [bk])] ++ dBase
  | bk `elem` currentBooks = dBase
  | otherwise = [(pers, [bk] ++ currentBooks)] ++ rest
  where
    currentBooks = books dBase pers
    rest = [pair | pair <- dBase, fst pair /= pers]

-- | Returns the updated database after a person
-- returns a book
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  | null currentBooks = dBase
  | not (bk `elem` currentBooks) = dBase
  | length currentBooks == 1 = rest
  | otherwise = [(pers, notReturned )] ++ rest
  where
    currentBooks = books dBase pers
    rest = [pair | pair <- dBase, fst pair /= pers]
    notReturned = [book | book <- currentBooks, book /= bk]

  
-- | Example database
exampleDb :: Database
exampleDb = [("Alice", ["Tintin", "Asterix"]), 
             ("Anna", ["Little Women"]), 
             ("Rory", ["Tintin"])]


-----------------------------------------------------------

{-

If we change the database representation from a list of
individual loans to a list of people with all their 
borrowed books in a list, then all the core functions need
to be adapted to handle lists of books per person instead
of one (Person, Book) pair per loan. 
The upsides are that the database is more compact
(especially if people borrow many books) and that we can
easily see all books a person has borrowed in one place.
The downsides are that we need to do more list processing
overall, and that we need to be careful to avoid duplicate
entries. The definitions for makeLoan and returnLoan are
more complex than before.

Overall the code looks a bit clunky, but that is mostly
due to the fact that we have not covered more advanced
list processing functions yet.


Testing in GHCi
ghci> :l Ex5_32
ghci> books exampleDb "Alice"
["Tintin","Asterix"]
ghci> borrowers exampleDb "Tintin"
["Alice","Rory"]
ghci> numBorrowed exampleDb "Alice"
2
ghci> borrowed exampleDb "Tintin"
True
ghci> db = makeLoan exampleDb "Rory" "Steppenwolf"
ghci> books db "Rory"
["Steppenwolf","Tintin"]
ghci> db' = returnLoan db "Rory" "Tintin"
ghci> books db' "Rory"
["Steppenwolf"]

-}