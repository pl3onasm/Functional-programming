module Chapter_05.Ex5_28 where  

-----------------------------------------------------------
-- Exercise 5.28

type Person = String
type Book = String

type Database = [(Person , Book)]

-- | Returns the books that a person has borrowed, if any
books :: Database -> Person -> [Book] 
books dBase findPerson
  = [book | (person, book) <- dBase, person == findPerson]

-- | Returns the borrowers for a given book, if any
borrowers :: Database -> Book -> [Person]
borrowers dBase findBook
  = [person | (person, book) <- dBase, book == findBook]

-- | Returns if a book is borrowed or not
borrowed :: Database -> Book -> Bool
borrowed dBase findBook
  = [] /= borrowers dBase findBook

-- | Returns the number of books a person has borrowed
numBorrowed :: Database -> Person -> Int
numBorrowed dBase findPerson 
  = length (books dBase findPerson)

-- | Makes a loan of a book to a person and returns
-- the updated database
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [(pers,bk)] ++ dBase

-- | Returns the updated database after a person
-- returns a book
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  = [pair | pair <- dBase, pair /= (pers,bk)]

-- | Example database
exampleDb :: Database
exampleDb = [("Alice", "Tintin"), ("Anna", "Little Women"), 
             ("Alice", "Asterix"), ("Rory", "Tintin")]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :l Ex5_29
ghci> books exampleDb "Anna"
["Little Women"]
ghci> borrowers exampleDb "Tintin"
["Alice","Rory"]
numBorrowed exampleDb "Alice"
2
ghci> borrowed exampleDb "Tintin"
True
ghci> borrowed exampleDb "Demian"
False


-}




