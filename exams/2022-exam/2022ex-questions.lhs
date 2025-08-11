-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2022               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the most general type of the following expression?

[([],[]), ([],[])]

--------
Answer: 



--------------------------------
Question 1.2:
What is the most general type of the following expression?

[([],"ABC"), ("DEF",[])]

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the function f?

f g = \ (a,b) -> g a b

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the standard Haskell function foldr?

g = not.not

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

f = \x -> \y -> \z -> (x (x y), x z)

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

n number theory, a Leyland number is a number of the form 
xy + yx, where x and y are integers greater than 1. 
The first few Leyland numbers are: 
  8, 17, 32, 54, 57, 100, 145, 177, 320, 368, 
  512, 593, 945, 1124

Write a Haskell function leyland :: Integer -> [Integer] 
such that leyland n yields the ascending list of Leyland 
numbers xy + yx, where 1 < x ≤ n and 1 < y ≤ n. 
Note that the list should not contain any duplicates.

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
ake use of the function map to define a function ahead 
(including its type) which takes a value x of some type, 
and a list of lists of that same type, and it returns the 
list of lists that is obtained by placing x at the front of
every component list. 

For example: 

  ahead 7 [[1,2], [], [3]] = [[7,1,2], [7], [7,3]]

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Consider the following Haskell function digits:

digits n = if n < 10 
           then [n] 
           else n `mod` 10 : digits (n `div` 10)

The function returns the digits of an Integer in reverse 
order, i.e. digits 120 = [0,2,1]. Using the higher-order 
function foldr, define a function dig2int (including its 
type) which is the inverse of the function digits.
So, dig2int (digits 120) = 120.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Using function composition (.), foldr, and filter write a 
function reveven (including its type) which takes a list 
of integers, removes all odd numbers, and reverses the 
result. So, reveven [1..10] = [10,8,6,4,2].

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Give an implementation of the standard Haskell function map 
(including its type) as a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Write a function unary that converts a list of non-negative 
integers into a list of unary 'numbers'. In the unary number 
system, an integer n is represented by a list containing 
n 1s. Your implementation must make use of a list 
comprehension.

As an example:
  unary [1,2,3,5] = [[1],[1,1],[1,1,1],[1,1,1,1,1]]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Consider the following function sum4:

  sum4 (w:ws) (x:xs) (y:ys) (z:zs) = 
    w+x+y+z:sum4 ws xs ys zs
  sum4 _ _ _ _ = []

Give an equivalent implementation of sum4 that does not use
recursion, but the function zipWith instead.

--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a Haskell definition for the infinite list 
ups=[1,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,..].

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
The Pell numbers P (n) are defined by the recurrence
relation:

  P (0) = 0 
  P (1) = 1
  P (n) = 2P (n - 1) + P (n - 2) for n > 1

Give an expression for the infinite list pells of Pell 
numbers that makes use of zipWith, map, and tail. So:

  take 6 pells = [0,1,2,5,12,29]

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Give a Haskell definition for the infinite list natpairs of
pairs (x, y), where x and y are natural numbers such that
x ≥ y. An additional requirement is that if (m,n) is a pair
of natural numbers with m ≥ n, then elem (m,n) natpairs
should return True in finite time. An implementation of 
natpairs that violates this requirement is considered a
non-solution.

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) Fifo tp implements a simple 
data type for the storage of elements of the type tp, from 
which elements are retrieved in the same order as in which 
they are inserted: FIFO stands for First In First Out 
queue.

Implement a module Fifo such that the concrete 
implementation of the type Fifo is hidden from the user.

The following operations on the data type Fifo must be 
implemented:
• empty returns an empty queue.
• isEmpty returns True for an empty queue, otherwise False.
• insert: returns the queue that is the result of inserting 
  an element.
• top: returns the 'oldest' element of the queue 
  (you may assume that the queue is non-empty).
• remove: returns the queue that is obtained by removing 
  the 'oldest' element (you may assume that the queue is
  non-empty).

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Prove: 

  foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs 
  for all finite lists xs and ys.

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the function mapTree:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f Empty = Empty
  mapTree f (Node x t1 t2) = 
    Node (f x) (mapTree f t1) (mapTree f t2)

Prove for all finite trees t: 
  
  mapTree f (mapTree g t) = mapTree (f.g) t

--------
Answer:







___________________________________________________________