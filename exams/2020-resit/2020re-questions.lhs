-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2020               
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

[not,(&& True)]

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[[(*)],(+)]

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

(42 - ) . (+ (42::Int))

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following function g?

g f = (:).f

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function f?

f = \ (x,y) z -> (x (x y), x z)

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Implement a function 

  longestPalsub :: Eq a => [a] -> [a] 
  
such that the call longestPalSub xs returns the longest 
subsequence of xs which is a palindrome. Here, a 
subsequence consists of a consecutive run of elements from 
xs. The time complexity of your solution should not exceed 
O(n^3), where n is the length of xs. You are not allowed to 
use the indexing operator (!!). 

For example:  

  longestPalsub "Be careful to step on no pets he said."

should return: " step on no pets " 
  
  longestPalsub [3,1,4,1,5,9,2,6,5] 
  
should return: [1,4,1]

--------
Answer:

> longestPalsub :: Eq a => [a] -> [a] 
> 
> 
> 
>
>



___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function splitWhen (including its type) which takes 
a predicate p and a list xs and returns a tuple (x,ys,zs) 
such that p x is True, xs=ys++[x]++zs, and p y is False for 
all y in ys. You may assume that p x is True for at least 
one element of xs. 
For example, 

  splitWhen even [1,3,4,5,2,1] 

  should return: (4,[1,3],[5,2,1])

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Give an implementation (and its type) of the standard 
Haskell function curry.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Implement a funtion map2 (including its type) which takes
a function f and a list of lists xss and outputs the list 
of lists that is obtained by applying f to the elements of 
the lists in xss. For example:

  map2 (*2) [[],[1,2],[5,6]]
  
  should return: [[],[2,4],[10,12]]

--------
Answer:

>
>
>


--------------------------------
Question 3.4:
The function count is recursively deﬁned as:

count _ [] = 0
count p (x:xs)
  |p x = 1 + count p xs
  |otherwise = count p xs

Give an implementation of count (including its type) that
does not use recursion nor a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 3.5:
Implement the function reverse using foldr.

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function isSorted :: [Int] -> Bool such that 
isSorted xs is True if and only if the list xs is ascending 
(i.e. each element is less or equal to its successor). 
Make use of a list comprehension together with the function 
and. For example, isSorted [1,2,3,3,4] should return True 
while isSorted [2,1] should return False.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Use a list comprehension to implement the function 
locations :: Eq a => a -> [a] -> [Int] which takes an 
item x and a list xs ands returns a list of indexes at 
which x is found in xs. Note that the ﬁrst element of a 
list has index 0. For example:

  locations 1 [3,1,4,1,5,9,2,6,5,1] 
  should return: [1,3,9] 

You are not allowed to use the indexing operator (!!).

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Given is the following function fun.

fun p n = concat (map f (filter p [1..n]))
  where f x = map (\y -> (x,y)) [1..x]

Give an equivalent implementation using a list 
comprehension. 
You are not allowed to use concat, filter or map.
Also, give the type of the function fun.

--------
Answer:

>
>
>


--------------------------------
Question 4.4:
Matrices can be represented in Haskell as lists of lists. 
For example, [[1,2,3],[4,5,6]] represents the 2 x 3
matrix of which the ﬁrst row is [1,2,3] and the second row 
is [4,5,6]. Write a function transpose that takes a matrix 
(i.e. a lists of lists) and returns the transposed matrix. 
For example:

  transpose[[1,2,3],[4,5,6]] 
  should return: [[1,4],[2,5],[3,6]] 

Your solution must make use of list comprehensions combined 
with recursion. You may assume that the input matrix is 
rectangular (i.e. each row has the same length). You are 
not allowed to use the indexing operator (!!).

--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Given is the inﬁnite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

Use it to deﬁne the inﬁnite list composites::[Integer] 
which is the list of all positive integers which are not 
prime.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Using zip or zipWith, give a deﬁnition of the inﬁnite list 
fs which is the list of numbers which are deﬁned as:

  F (0) = 0 
  F (1) = 1
  F (n) = 2F (n - 1) + F (n - 2) for n ≥ 2

So, the expression take 10 fs equals 
  [0,1,2,5,12,29,70,169,408,985]. 
  
Your implementation should be such that take n fs has 
an O(n) time complexity.

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Implement the ordered inﬁnite list ds23 of all positive 
integers that can be expressed as 2i · 3j (where i and j 
are non-negative integers). For example, take 15 ds23 
equals [1,2,3,4,6,8,9,12,16,18,24,27,32,36,48].

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type Fifo a is an Abstract Data Type (ADT) for FIFO 
queues containing elements of type a. Recall that a Fifo 
queue is a container that works according the First-In-
First-Out principle. Implement a module Fifo which exports
the abstract data type but hides the concrete 
implementation. You may choose yourself a suitable data 
representation for Fifo queues.

The following operations on queues need to be implemented:
• empty: returns an empty queue.
• isEmpty: returns True for an empty queue, 
  otherwise False.
• insert: inserts an element in a Fifo queue.
• retrieve: returns the 'oldest' element from 
  a non-empty Fifo queue.
• delete: returns the ﬁfo that is obtained by removing 
  the 'oldest' element from the queue.
• size: returns the number of elements of the Fifo queue.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell functions:

f xs ys zs = g xs (ys ++ zs)

g [] ys = []
g (x:xs) ys = ys ++ g xs ys

Prove that 

  length (f xs ys zs) = 
    length xs * length ys + length xs * length zs 

for all ﬁnite lists xs, ys, and zs.

[Note: definitions of length and ++ are given in the file
 functions.md]

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

Prove for all ﬁnite trees t: 

  reverse(inorder(mirror t)) = inorder t

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately. You may use 
 without proof that ++ is an associative operator, and 
 that xs ++ [] = xs.
 The definition of ++ is given in functions.md]

--------
Answer:







___________________________________________________________