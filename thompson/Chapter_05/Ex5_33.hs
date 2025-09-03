import Chapter_05.Ex5_32
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 5.33

-- | Property 1: If we loan bk to pers and then lookup the 
-- books loaned to pers, then bk should be in that list.
property_loaned :: Database -> Person -> Book -> Bool
property_loaned dBase pers bk =
  elem bk loanedAfterLoan == True
    where
      afterLoan = makeLoan dBase pers bk
      loanedAfterLoan = books afterLoan pers


-- | Property 2: If we return the loan of bk by pers and
-- then lookup the books loaned to pers, then bk should
-- not be in that list.
property_notLoaned :: Database -> Person -> Book -> Bool
property_notLoaned dBase pers bk =
  elem bk notLoanedAfterReturn == False
    where
      afterReturn = returnLoan dBase pers bk
      notLoanedAfterReturn = books afterReturn pers


-----------------------------------------------------------


{-

Both QuickCheck properties can be kept as they are. This is
because the signatures of the functions have not changed. 
They behave as before, even though the internal 
representation of the database has changed. From the point 
of view of the tests, nothing has changed: the functions 
still accept a database, a person and a book as arguments 
and return the same results. In other words, the tests are 
agnostic to the internal representation of the database.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex5_33
ghci> quickCheck property_loaned
+++ OK, passed 100 tests.
ghci> quickCheck property_notLoaned
+++ OK, passed 100 tests.

-}