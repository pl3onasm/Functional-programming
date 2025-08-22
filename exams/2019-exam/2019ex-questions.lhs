-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2019               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression type correct? 
If YES, then give the type of the expression.

'a' : 'b' : [] : []

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

('a' : 'b' : []) : []

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the following function f?

f = (\x -> \y -> \z -> [x y, x (x z)])

--------
Answer: 



--------------------------------
Question 1.4:
What is the most general type of the following function g?

g = .not

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function h?

h = not.

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

A well formed string of parentheses is deﬁned by the 
following recursive rules:

• The empty string is well formed.
• If s is a well formed string, 
  then (s) is a well formed string.
• If s and t are well formed strings, 
  then their concatenation st is a well formed string.

For example, "((()))" and "()()()" are well formed strings, 
while "(()", ")(()" and ")(" are not. 

Write a Haskell function isWFS :: String -> Bool such that
isWFS str returns True if the string str is well formed and
False otherwise.

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
Without using recursion or a list comprehension, write a 
function selectiveMap which takes three arguments. Also, 
give the type of the function selectiveMap. 

The ﬁrst argument of the function is a predicate p, the 
second some function f, and the third a list xs. The 
function selectiveMap returns a list that is just like xs, 
but in which every element x that satisﬁes p is replaced 
by f applied to x.

For example, the call 

  selectiveMap even (*2) [1,2,3,4,5,6] 
  should return: [1,4,3,8,5,12]

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Without using recursion or a list comprehension, write a 
function thresholdPairs which takes two arguments.
The ﬁrst is an Integer n, and the second is a list xs of 
Integer pairs. The output should be the list of pairs 
(a,b), in the same order as in the list xs, for which the
sum of a and b is greater than n.

For example, the function call 

  thresholdPairs 3 [(1,2),(2,2),(3,5),(0,3),(0,4)] 
  should return: [(2,2),(3,5),(0,4)]

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Implement the standard function map using the standard 
function foldr.

--------
Answer:

>
>
>



___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Use a list comprehension to implement the function 
partition which takes two arguments. The first is some
element x, and the second a list xs. The function should 
return a pair of lists of which the first is the list of 
all elements of xs that are less than or equal to x, while 
the second is the list of all elements of xs that are 
greater than x. Also, give the most general type of the 
function partition.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Use an efficient list comprehension to implement the 
function tripletSum (including its type) that takes a 
positive Integer n, and returns the lexicographically 
ordered list of all triples (a,b,c) such that n equals 
a+b+c and 1 <= a <= b <= c. 

For example:

  tripletSum 6 = [(1,1,4),(1,2,3),(2,2,2)]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
The function adjacentTriples takes a list xs and outputs
the list of all triples of adjacent elements in the list 
xs. Give its type and an implementation using a list 
comprehension. 

For example:

  adjacentTriples "curry"
  = [('c','u','r'),('u','r','r'),('r','r','y')]

--------
Answer:

>
>
>



___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a recursive implementation of the function iterate 
(including its type) that takes two arguments. The first is 
a function f and the second some value x. The call 
iterate f x returns an infinite list of repeated 
applications of f to x. So: 

iterate f x = [x, f x, f(f x), f(f(f x)), f(f(f(f x))),...]

For example: 

  take 10 (iterate (*2) 1)
  = [1,2,4,8,16,32,64,128,256,512]

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Give a deﬁnition of the infinite list tribonacci which is 
the ordered list of all tribonacci numbers. Recall that the
tribonacci numbers are deﬁned as: 

  T (n) = n for n < 3
  T (n) = T (n - 1) + T (n - 2) + T (n - 3) for n ≥ 3

So: take 10 tribonacci = [0,1,2,3,6,11,20,37,68,125] 

Your implementation must make (useful) use of the function 
zipWith, so map T [0..] is not accepted as a valid answer.


--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Give a deﬁnition of the infinite list palindromes which is 
a list of lists of palindromic bit strings. The n-th list
contains all lexicographically sorted palindromes of length 
n (starting with n = 0). For example: 

  take 4 palindromes
  = [[""],["0","1"],["00","11"],["000","010","101","111"]]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type Polynomial is an Abstract Data Type (ADT) for real 
valued polynomials.

Implement a module Polynomial such that the implementation 
of the type Polynomial is hidden to the user.

The following operations need to be implemented:
• makePolynomial coeffs converts the coeffcients in the 
  list coeffs into a Polynomial. For example,
  makePolynomial [2.0,0.0,0.5] should produce the 
  Polynomial representation of 2x^2 + 0.5.
• eval pol x returns the evaluation of the polynomial pol 
  at x. For example, 2x^2 + 0.5 at x = 1.0 can be computed
  using eval (makePolynomial [2.0,0.0,0.5]) 1.0.
• add lhs rhs: returns the polynomial that is the addition
  of lhs and rhs. For example, (2x^2 + 0.5) + (x - 1) can
  be constructed using 
  add (makePolynomial [2.0,0.0,0.5]) 
      (makePolynomial [1.0,-1.0])
• scale a pol: multiplies the polynomial pol by the scalar 
  a. For example, 5(2x^2 + 0.5) can be constructed using
  scale 5.0 (makePolynomial [2.0,0.0,0.5])

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell function:

    f 0 = 0
    f 1 = 1
    f n = 5*(f (n-1)) - 6*(f (n-2))

Prove the following property: 
  
    f n = 3^n - 2^n 
    for all non-negative integers n.

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and flatten:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  flatten :: Tree a -> [a] -> [a]
  flatten Empty ys = ys
  flatten (Node x l r) ys = flatten l (x:flatten r ys)

Prove for all ﬁnite trees t: 

  inorder t = flatten t []

[Note: If you need one or more lemmas to complete the 
proof, then prove these lemmas separately.
The defintion of ++ can be found in functions.md]

--------
Answer:







___________________________________________________________