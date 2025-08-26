-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2018               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  True : [] : []

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True: []) : []

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True: []) : [] ++ [False]

--------
Answer: 



--------------------------------
Question 1.4:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True : []) : [] ++ [[False]]

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function f?

  f = map . filter

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

This problem is about pattern matching. A pattern is a 
String that specifies (describes) the strings that match 
the pattern. A pattern may only consist of lower case 
letters from the alphabet (i.e. a..z), asterisks (i.e. 
the * character), and question marks (i.e. the ? char). 
A question mark may only follow a letter and indicates zero
or one occurrence of the preceding character. For example, 
colou?r matches both color and colour. An asterisk may only 
follow a letter and indicates zero or more occurrences of 
the preceding character. 
For example, ab*c matches ac, abc, abbc, abbbc, and so on.

Write a Haskell function 

isMatch :: String -> String -> Bool 

such that isMatch pat str returns True if and only the 
string str can be produced by the pattern pat. 
For example: 

  isMatch "h?i?el*o?" "hello" = True
  isMatch "h?iel*" "ill" = False.

--------
Answer:

>
> 
> 
> 
>
>



___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Use the higher-order function foldr to implement the 
function factorial (including its type) which takes a non-
negative integer n and return the factorial of n 
(i.e. n*(n-1)*(n-2)*...*1). 

Example:  factorial 5 = 120

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
The higher order function foldr is used for reducing a list
as in the following example:

  foldr f 0 [1..5] = f 1 (f 2 (f 3 (f 4 (f 5 0))))

Implement the 'mirror' operation foldl (including its type)
such that foldl f 0 [1..5] = f (f (f (f (f 0 1) 2) 3) 4) 5

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Using function composition (.), foldr and the cons operator 
(:), implement the function folmap (including its type), 
which is your version of the standard function map. 

Example:   folmap (*2) [1,2,3,4] = [2,4,6,8]

--------
Answer:

>
>
>



___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function pairs (including its type) using a 
list comprehension. 

Example:  pairs [1..3] ['a','b']
        = [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Use a list comprehension to implement the function 
locations (including its type) that takes a value of some
type and a list of that type, and returns a list with 
locations (indexes starting from zero) where the value 
occurs in the list.

For example:    locations 1 [1,0,1,0,4,1] = [0,2,5]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
The function sumProdPairs = zipWith (\x y -> (x+y,x*y)) 
is defined using the function zipWith.
Give an equivalent definition of sumProdPairs that uses 
a list comprehension instead.

--------
Answer:

>
>
>



___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a definition of the Haskell function repeat (including 
its type) that takes an argument and produces the list that
indefinitely repeats that argument. 

Example:    repeat 42 = [42,42,42,42,42,42,....]

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Give a definition of the infinite list binaries which is 
the list of all non-empty lists containing zeros and ones.
The order of the elements of the list should be as in the 
following example: 

    take 14 binaries
    = [[0],[1],[0,0],[1,0],[0,1],[1,1],[0,0,0],
       [1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],
       [0,1,1],[1,1,1]]

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Consider (x + 1)^n, for integer n ≥ 0. We can write this in
coefficient normal form, i.e. in the form:

      aₙx^n + aₙ₋₁x^{n-1} + .. + a₀
    
For example, (x + 1)^4 = x^4 + 4x^3 + 6x^2 + 4x + 1, yields 
the list of coefficients [1,4,6,4,1]. 
  
Give a definition of the infinite list coefficients of 
lists of coefficients, such that the n-th list corresponds 
with the coefficients of (x + 1)^n.

For example: 

    take 5 coefficients 
    = [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type Peano is an Abstract Data Type (ADT) for 
implementing natural numbers as follows:

  • Zero is a constructor that represents 
    the natural number 0.
  • Succ n, where n is of the type Peano, represents 
    the number that is 1 greater than the number 
    that n represents.

Implement a module Peano such that the concrete 
implementation of the type Peano is hidden to the user.

The following operations on Peano numbers need to be 
implemented:

• peanoToInteger n converts the Peano number n 
  into its decimal Integer value.
• isZero n returns True if and only if the 
  peano Number n represents 0.
• isLessThan a b: returns True if and only if 
  the Peano number a is less than the Peano number b.
• plus a b: returns the Peano representation of 
  adding the Peano numbers a and b.
• mul a b: returns the Peano representation of 
  multiplying the Peano numbers a and b.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell functions.

    f 0     = 0
    f 1     = 1
    f n     = f (n-1) + f (n-2)
    
    g 0 a b = a
    g n a b = g (n-1) b (a+b)

Prove the following property p:

    f n = g n 0 1 
    for all non-negative integers n

--------
Answer:




___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions 
foldT, mapT, and inorder:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  foldT :: (a -> a -> a) -> a -> Tree a -> a
  foldT f z Empty = z
  foldT f z (Node x l r) = 
      f (f (foldT f z l) x) (foldT f z r)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = 
      inorder l ++ [x] ++ inorder r

Let f :: a -> a -> a be an associative function 
(i.e. f a (f b c) = f (f a b) c) with identity element z 
such that f x z = f z x = x.

Prove for all finite trees t: 

  foldT f z t = foldr f z (inorder t)

[Note: You may use that the operator ++ is associative 
 without giving a proof.]

--------
Answer:




___________________________________________________________