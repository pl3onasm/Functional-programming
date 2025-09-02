-----------------------------------------------------------
-- Exercise 5.30

type Person = String
type Book = String

data Loan = Loan Person Book
  deriving (Eq, Show)

type Database = [Loan]

-- | Returns the books that a person has borrowed, if any
books :: Database -> Person -> [Book] 
books dBase findPerson
  = [book | Loan person book <- dBase, person == findPerson]

-- | Returns the borrowers for a given book, if any
borrowers :: Database -> Book -> [Person]
borrowers dBase findBook
  = [person | Loan person book <- dBase, book == findBook]

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
makeLoan dBase pers bk = [Loan pers bk] ++ dBase

-- | Returns the updated database after a person
-- returns a book
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  = [loan | loan@(Loan person book) <- dBase,
            not (person == pers && book == bk)]

-- | Example database
exampleDb :: Database
exampleDb
  = [Loan "Alice" "Tintin", Loan "Anna" "Asterix",
     Loan "Alice" "Little Women", Loan "Rory" "Tintin"]


-----------------------------------------------------------

{-

If we change the representation of loans from pairs to a
custom data type, then all functions that manipulate loans
need to pattern match on the Loan constructor instead of
tuple patterns. The new definitions are very similar to
the old ones, but the code is more readable and the type
signatures are less error-prone: you cannot accidentally
swap a Person and a Book anymore.

Testing in GHCi

ghci> :l Ex5_30
ghci> books exampleDb "Alice"
["Tintin","Little Women"]
ghci> borrowers exampleDb "Tintin"
["Alice","Rory"]
ghci> numBorrowed exampleDb "Alice"
2
ghci> borrowed exampleDb "Tintin"
True

-}