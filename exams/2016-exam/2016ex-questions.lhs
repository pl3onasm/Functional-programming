-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2016               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following expression?

(42, [42], [[42]])

--------
Answer: 



--------------------------------
Question 1.2:
What is the most general type of the function f?

f = filter (== 'A')

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the function g?

g = (\x -> (\y -> (y,x)))

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the function foldr?

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

h =(\f -> map f "Text" == [1,2,3,4])

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

The increasing list [1,2,3,4,5] has 9 non-empty increasing 
sublists that contain as many even numbers as odd numbers.

Write a Haskell function balancedSublists (including its 
type) that takes an increasing list and returns the list of 
its non-empty increasing sublists that have as many even 
numbers as odd numbers. The order of the sublists is 
irrelevant.

For example, balancedSubLists [1,2,3,4,5] 
may return the list:

  [[4,5],[3,4],[2,5],[2,3],[2,3,4,5],
  [1,4],[1,2],[1,2,4,5],[1,2,3,4]].

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
Write a function isEqual (including its type) that accepts 
three arguments: the ﬁrst two arguments are functions
(both having the same type), which can be applied to each 
element of a list (the third argument). The function should
return True if and only if applying both functions to each 
element of the third argument yields the same result.
For example, isEqual (+1) (1+) [1,2,3] should yield True, 
while isEqual (^2) (2^) [1,2,3] should yield False. 
Your are not allowed to use recursion.

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
The function concat concatenates the elements of a list 
of lists. For example:

  concat [[1,2],[3],[4,2,3]] = [1,2,3,4,2,3] 

Give an implementation of the function concat (including 
its type) using foldr.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Write a function mulinceven (including its type) that takes
a list of Integers, and returns the product of one plus 
every number in the input that is at least 4. 
For example, mulinceven [7,3,2,4,5] returns 240, because
(7+1)*(4+1)*(5+1) = 240. Your implementation must make use 
of map, filter, and foldr.

--------
Answer:

>
>
>



___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Write a function oddeven (including its type) that takes a 
list of pairs and returns a list containing the ﬁrst 
element from each of the pairs in even-numbered positions 
and the second element from each of the pairs in odd-
numbered positions, where numbering of list elements begins 
from 0.

Examples:
            oddeven [(1,2),(3,4),(5,6),(7,8)] 
          = [1,4,5,8]
            
            oddeven [("hello","world"),("from","Venus")] 
          = ["hello", "Venus"].

The implementation off oddeven must be a list 
comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Write a function removeRepetition (including its type) 
that removes all but one occurrence of consecutive  
repeated elements from its input list.

Examples:   removeRepetition [1,2,2,3,3,3,4,5,1,1]
          = [1,2,3,4,5,1]
          
            removeRepetition "Haaassskkkell"  
          = "Haskel"

The deﬁnition of the function removeRepetition must make 
use of a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Write a function sublists (including its type) that takes
a list and returns the list of all its possible sublists 
(the order of the sublists is irrelevant). 
Use a list comprehension in combination with recursion.
For example:
  sublists [1,2,3] may return 
  [[],[1],[2],[3],[1,2],[1,3],[2, 3],[1,2,3]]

--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Given the availablility of the inﬁnite list of prime
numbers, defined as follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  
Write a function isPrime such that isPrime n returns True 
if and only if n is in the list primes.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
The inﬁnite list ones is deﬁned as ones = 1 : ones.
Use only ones, arithmetic operators, and zipWith to create 
two mutually recursive deﬁnitions of the inﬁnite lists
evens and odds, where evens = [0,2,4,6,8,..] and 
odds = [1,3,5,7,9,..]. Mutual recursive means that evens 
(but not odds) can appear in the deﬁnition of odds and 
odds (but not evens) can appear in the deﬁnition of evens.

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Deﬁne the function multiples :: [Integer] -> [Integer], 
that takes a ﬁnite list of Integers and produces the 
inﬁnite sorted list (without repetitions) of all 
multiples of the numbers in the input list.

For example: 
    take 10 (multiples [2,3,5]) 
  = [0,2,3,4,5,6,8,9,10,12].

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) Set tp implements a data type 
for the storage of sets of the type tp, where tp is of the 
class Ord (i.e. the elements are ordered).

Implement a module Set that exports the ADT Set. You can 
choose a concrete implementation yourself, however this
implementation must be hidden from the user of this module.

The following operations on the data type Set must be 
implemented:
• empty returns an empty set.
• isEmpty returns True for an empty set, otherwise False.
• insert: returns the set after insertion of an element.
• delete: returns the set after removal of an element.
• union: returns the union of two sets.
• intersection: returns the intersection of two sets.

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Given is the recursive deﬁnition of the function drop:

  drop :: Int -> [a] -> [a]
  drop 0 xs = xs
  drop n [] = []
  drop n (x:xs) = drop (n-1) xs

Prove the following property p: 

  drop m (drop n xs) = drop (m+n) xs 
  for all ﬁnite lists xs and m, n ≥ 0

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions lrorder, 
rlorder, and mirror:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  mirror :: BinTree a -> BinTree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  lrorder, rlorder :: BinTree a -> [a]
  lrorder Empty = []
  lrorder (Node x l r) = lrorder l ++ [x] ++ lrorder r

  rlorder Empty = []
  rlorder (Node x l r) = rlorder r ++ [x] ++ rlorder l

Prove for all ﬁnite trees t: 

  lrorder t = rlorder (mirror t)

--------
Answer:







___________________________________________________________