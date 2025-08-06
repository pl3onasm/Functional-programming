-----------------------------------------------------------
-- Exercise 2.3

n :: Int
n =  a `div` length xs
     where
       a = 10
       xs = [1,2,3,4,5]

-----------------------------------------------------------

{-
  The given code snippet was as follows:
  
      N =  a 'div' length xs
           where
              a = 10
             xs = [1,2,3,4,5]

  The syntactic errors in this code are:

  1. The variable N should be lowercase to follow 
     Haskell's naming conventions for variables. This has
     been corrected to n.

  2. The 'div' operator should be used with backticks when
     it is used in infix notation. In Haskell, backticks
     allow functions to be used as infix operators. 

  3. The local definitions inside the 'where' clause must  
     be consistently indented. Both 'a' and 'xs' must  
     align vertically as part of the same block.

  Things to note:

  1. Parentheses around 'length xs' are not necessary. This
     is because function application (like: length xs) has 
     higher precedence than any operator, including 'div'.
     Thus, the expression is parsed as expected.

  2. While a type signature is optional, including one 
     (like  n :: Int) is considered good practice for 
     readability and type clarity.

-}