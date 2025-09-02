import Chapter_05.Ex5_28
import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 5.31

property_loan :: Database -> Person -> Book -> Person 
                 -> Book -> Bool
property_loan dBase pers bk pers2 bk2 =
  elem pers (borrowers dBase bk) ==
  elem pers (borrowers (makeLoan dBase pers2 bk2) bk)

property_loan' :: Database -> Person -> Book -> Person 
                  -> Book -> Property
property_loan' dBase pers bk pers2 bk2 =
  pers /= pers2 || bk /= bk2 ==>
  elem pers (borrowers dBase bk) ==
  elem pers (borrowers (makeLoan dBase pers2 bk2) bk)


-----------------------------------------------------------


{-

The statement we want to prove is the following:
If a particular person has not borrowed a particular book
bk and we make a loan of a random book to a random person,
then the first person still has not borrowed the particular
book bk. 

The first property is a direct translation of this
statement into code. If the person pers has borrowed or not
the book bk should not be affected by making a loan of a 
random book bk2 to a random person pers2. Therefore, the 
property should hold in both cases: 

    True == True and False == False.

However, if the random person pers2 happens to be the same
as pers and the random book bk2 happens to be the same
as bk, then the property does not hold: False /= True.
To avoid this situation, we add a precondition to the
property, which is expressed using the operator ==>.
The precondition states that either the two persons are
different or the two books are different. In this way,
we avoid the situation where both the persons and the books
are the same.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex5_31
ghci> quickCheck property_loan
*** Failed! Falsified (after 1 test):  
[]
""
""
""
""
ghci> quickCheck property_loan'
+++ OK, passed 100 tests; 11 discarded.

Indeed, the first property fails as explained above, while
the second property holds. The number of discarded tests
is the number of times the precondition was not satisfied.
The test case that falsified the first property is the one
where both persons and both books are the empty string.


-}