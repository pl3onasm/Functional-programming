-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2015               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell expression?

  [('4'=='4', 2)]

--------
Answer: 



--------------------------------
Question 1.2:
What is the most general type of the Haskell function f?

  f = map length

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the Haskell function g?

  g = foldr (+) 0

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following Haskell expression?

  (\f -> (\g -> (\x -> f (g x))))

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

  h = head . tail . fst

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Write a Haskell function perms (including its type) that 
accepts as its argument a list, and outputs the list of 
lists that are the permutations of the input list. 
For example: 

    perms "abc" 
  = ["abc","acb","bac","bca","cab","cba"]

Note that the order of the elements in the output list 
is not important.

--------
Answer:

>  
> 
> 
> 
>
>



___________________________________________________________

3. List comprehensions
___________________________________________________________

Question 3.1:
Implement the function filter (including its type) as a
list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
A Pythagorean triad is a triple of positive integers 
(a, b, c) such that a^2 + b^2 = c^2. Write a Haskell 
function pytriads n that returns a list of all Pythagorean
triplets such that 0 < a ≤ b ≤ c ≤ n. This list must be 
ordered based on the value of c.

For example: 
              pytriads 18 
            = [(3,4,5),(6,8,10),(5,12,13),
               (9,12,15),(8,15,17)]

The implementation of pytriads must be a list
comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
An integer n is called a perfect number if it is equal to 
the sum of all its proper divisors (excluding n itself). 
For example, 6 is a perfect number, since 6 = 1+2+3. 
An integer n is called a semi-perfect number if it is equal
to the sum of a subset of its proper divisors (excluding n 
itself). An example of a semi-perfect number that is not a 
perfect number is 12, since the proper divisors of 12 are 
1, 2, 3, 4, and 6 which add up to 16, but 2+4+6 = 12.

Write a Haskell function spf n that computes the list of 
all semi-perfect numbers in the range [1..n]. The implemen-
tation must be a list comprehension (which may use helper 
function if needed). 

For example:  spf 50 = [6,12,18,20,24,28,30,36,40,42,48]

--------
Answer:

>
>
>


___________________________________________________________

4. Infinite lists
___________________________________________________________

Question 4.1:
Give a recursive definition of the infinite list tf of non-
empty lists of alternating boolean values that end with the
value True. 

Example:      take 4 tf 
            = [[True], [False, True], [True,False,True], 
               [False,True,False,True]]

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Give a recursive definition of the list bits of all binary 
(character) strings that end with a zero, and do not 
contain two consecutive ones. The order of the strings may
be chosen arbitrarily.
For example, take 8 bits may yield 
["0","10","00","100","010","1010","000","1000"].

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Give a definition of the function multiples that takes as 
its argument a finite list of positive integers, and 
outputs the infinite list of all multiples of the input 
numbers. Note that the output list must be generated in 
increasing order, and should not contain any duplicates.

Example:    take 14 (multiples [3,5,7]) 
          = [0,3,5,6,7,9,10,12,14,15,18,20,21,24]

--------
Answer:

>
>
>


___________________________________________________________

5. ADT module
___________________________________________________________

The abstract data type Bag tp implements a simple data type
for the storage of elements of the type tp. A bag is also 
known as a multiset, since elements may occur multiple 
times. For example, if we insert some element twice in a 
bag, and remove one of them afterwards, then one element 
still remains in the bag (in contrast with a standard set).

Implement a module Bag such that the concrete 
implementation of the type Bag is hidden from the user.

The following operations on the data type Bag must be 
implemented:
• empty: returns an empty bag.
• isEmpty: returns True for an empty bag, 
  otherwise False.
• insert: returns the bag that is the result 
  of inserting an element.
• del: returns the bag that is obtained 
  by removing an element.
• cardinality: returns the number of occurrences 
  of an element in the bag.
• union: returns the union of two bags.
• intersect: returns the intersection of two bags.

--------
Answer:

> 
>
>


___________________________________________________________

6. Proof on lists
___________________________________________________________

The definitions of the functions filter, and (++) are given 
in the file functions.md of this exam folder.

Prove the following property p: 

  filter p (xs ++ ys) = filter p xs ++ filter p ys 
  for all finite lists xs and ys

[Note: refer to the file functions.md for the 
 definitions of filter and (++)]

--------
Answer:







___________________________________________________________

7. Proof on trees
___________________________________________________________

Given is the data type BinTree, and the functions inorder 
and maptree:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  inorder :: BinTree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  maptree :: (a -> b) -> BinTree a -> BinTree b
  maptree f Empty = Empty
  maptree f (Node x l r) = 
    Node (f x) (maptree f l) (maptree f r)

Prove for all finite trees t: 
  
  inorder(maptree f t) = map f (inorder t)

Associativity of (++) may be used without proof:

  (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

[Note: refer to the file functions.md for
 definitions of map and (++)]

--------
Answer:







___________________________________________________________