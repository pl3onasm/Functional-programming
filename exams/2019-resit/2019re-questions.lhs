-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2019               
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

  [True]:[]

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

  []:[True]

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the following function f?

  f = (\x -> \y -> \z -> [x (y z), y z])

--------
Answer: 



--------------------------------
Question 1.4:
What is the most general type of the following function g?

  g = \x -> \y -> \z -> x.y.z

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following function h?

  h = foldr (&&)

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

We call an Integer n a trinumber if n can be expressed as  
a sum of distinct powers of three (i.e. no duplicates of 
powers of three are allowed). 

For example, the numbers 1, 3, 9, 12, and 118 are all 
trinumbers because:

    1 = 3^0
    3 = 3^1
    9 = 3^2
    12 = 3^1 + 3^2
    118 = 3^0 + 3^2 + 3^3 + 3^4

Note that the number 20 can be expressed as a sum of powers 
of three as follows: 20 = 3^0 + 3^0 + 3^2 + 3^2, however 20 
is not a trinumber because the powers of three are not 
distinct.

Give a implementation of isTriNumber n (including its type) 
which returns True if and only if n is a trinumber.

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
Give an implementation of the function length that makes 
use of foldr.

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
The function aligned accepts two lists, and returns the 
number of aligned elements in the two lists. For example,
aligned "abca" "abdae" should return 3. 

Give an implementation of the function aligned that does 
not make use of recursion or a list comprehension. 
What is the type of the function aligned?

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
The function concatMap is defined as follows: 

  concatMap f xs = concat(map f xs)

Give an alternative implementation of concatMap using the
function foldr. What is the type of concatMap?

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What is the output of the expression 

  take 6 [(x,y) | x <- [1..], y <- [x+1..]]?

--------
Answer:



--------------------------------
Question 4.2:
The function evenLists is defined as: 

  evenLists xss = map (filter even) xss

Give an alternative implementation of this function using a 
list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
The function triples takes three finite lists and combines 
them as follows. 

Let: xs = [x0, x1, x2, .., xl], ys = [y0, y1, y2, .., ym], 
zs = [z0, z1, z2, .., zn], and q the minimum of l, m and n.

Then: triples xs ys zs = [(x0, y0, y0), (x1, y1, z1), 
                         (x2, y2, z2, ).., (xq , yq , zq )]

For example: 
              triples [0..3] [2..10] [3..20] =
              [(0,2,3),(1,3,4),(2,4,5),(3,5,6)]

Give the type of the function triples and an implementation 
using a list comprehension.

--------
Answer:

>
>
>


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Define the infinite list fibs of Fibonacci numbers using a 
list comprehension. So:
  
  take 10 fibs 
  should return: [0,1,1,2,3,5,8,13,21,34]
  
Note that fibs = [fib n | n <- [0..]] is not considered a 
valid answer.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Without using a list comprehension, give a definition of 
the infinite list natlists = [[0],[0,1],[0,1,2],...].

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Implement the function multiples that takes a finite list
of Integers and outputs the increasing infinite list of
positive integers that can be expressed as a multiple of 
one (or more) of the numbers in the input list.

For example:

  take 10 (multiples [5,2,8]) 
  should return: [2,4,5,6,8,10,12,14,15,16]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type Complex is an Abstract Data Type (ADT) for complex 
numbers.

Implement a module Complex such that the implementation of 
the type Complex is hidden to the user. Recall that the 
complex number a + ib (where i is the imaginary number for 
which i^2 = -1) can be represented as a pair (a, b) where a
and b are Doubles. 

The following operations need to be implemented:
• add: returns the complex addition of two complex numbers. 
  Recall that (a + ib) + (c + id) = (a + c) + i(b + d).
• sub: returns the complex subtraction. 
  Recall that (a + ib) - (c + id) = (a - c) + i(b - d).
• mul: returns the multiplication of two complex numbers. 
  Recall that (a + ib)(c + id) = (ac - bd) + i(ad + bc).

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Consider the following Haskell function rvl.

  rvl [] ys = ys
  rvl (x:xs) ys = rvl xs (x:ys)

Prove the following property:

  rvl (xs++ys) [] = rvl ys (rvl xs []) 
  for all finite lists xs and ys.

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and mirror:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

Prove for all finite trees t: 

  inorder (mirror t) = reverse (inorder t)

[Note: You may use without proof that the operator ++ is 
associative and that xs = xs ++ []. If you need any other 
lemmas to complete the proof, then prove these lemmas 
separately. For the definitions of reverse and ++ see the 
file functions.md]

--------
Answer:







___________________________________________________________