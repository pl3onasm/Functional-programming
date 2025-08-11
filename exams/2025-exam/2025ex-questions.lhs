-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2025               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

([[]],[])

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

[[[]],[]]

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

[[[]],[True]]

--------
Answer: 



--------------------------------
Question 1.4:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

not.(&&)

--------
Answer: 



--------------------------------
Question 1.5:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

(&&).not

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

As you know, merge sort is a divide-and-conquer sorting 
algorithm that sorts a list by splitting it into two 
halves, recursively sort each half, and then merge the 
sorted halves into a sorted list.

Give a Haskell implementation of the function 
mergeSort :: Ord a => [a] -> [a].

The call mergeSort [4,2,5,1,2] should return the sorted 
list [1,2,2,4,5]. You are not allowed to use the indexing 
operator (!!). You are allowed to imlement helper 
functions, but not your own implementation of the indexing
operator.

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
The function pairDiff takes a list xs of integers, and 
returns the list of pairwise differences x-y of each pair
(x, y) where x appears before y in the list xs. 
The implementation must be a list comprehension (and is not
allowed to use recursion).

For example:
  
  pairDiff [6,5,4,3,2,1] 
  should return: [1,2,3,4,5,1,2,3,4,1,2,3,1,2,1]

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Give an implementation of the function sublists xs 
(including its type) that returns the list of all 
contiguous sublists of xs. The implementation should 
make use of a list comprehension.

For example: 

  sublists [1,2,3] 
  should yield: [[1],[1,2],[1,2,3],[2],[2,3],[3]]


--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Give a definition of the function 
zigzag :: [a] -> [a] -> [a] that produces a list that is an
alternation of the elements from two input lists. This 
alternation stops when the shortest input list is 
exhausted. The implementation of zigzag must be a list 
comprehension (without recursion).

For example: 
  
  zigzag [1..5] [10..20] 
  must produce: [1,10,2,11,3,12,4,13,5,14]

--------
Answer:

>
>
>


___________________________________________________________

4. Higher-order functions
___________________________________________________________

Question 4.1:
Use foldr to implement the function glue, which acts the 
same as the ++ operator. So glue xs ys must return xs++ys.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Use foldr to implement the function mapf. The function mapf
must act the same as the standard map function, so 
mapf f xs must return the same result as map f xs.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Give the implementation and the type of the function doif
which takes a predicate, a function, and a list. Its output 
is the list that is obtained by applying the function to 
all elements of the list that satisfy the predicate, while 
all other elements remain unchanged. 

For example:
  doif (<=2) (*10) [1,2,3,4,5,1] = [10,20,3,4,5,10]

--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists and lazy evaluation
___________________________________________________________

Question 5.1:
Given the availablility of the inﬁnite list of prime
numbers, defined as follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  
Give the definition of the function isComposite n which 
returns True if and only if n is a composite 
(i.e. non-prime) number. You are not allowed to use the 
boolean operator not in your solution.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
The numbers 

  t(n) = ∑_{i=1}^n i = n(n+1)/2

(for positive integers n) are called triangle numbers. 
The expression Σ_{i=1}^n i is the sum of the first n
positive integers which is equal to n(n+1)/2. So, the 
n-th triangle number equals the sum of the first n 
positive integers.

Implement the ordered infinite list trinums of triangle 
numbers. 

So, take 8 trinums should return [1,3,6,10,15,21,28,36].

The implementation must have the form 

  trinums = ?? : [x + y | (x,y) <- ??]. 
  
You are required to substitute proper expressions for the
question marks.

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Consider the Cantor snake from the figure included in this
exam folder (file: CantorSnake.png).
We represent the rational number a b as the pair (a, b). 
Give a Haskell definition of the inifinite list cantor that 
produces all pairs in the order that is given in the 
figure. So:

  take 10 cantor = [(1,1),(1,2),(2,1), (3,1),(2,2),
                    (1,3),(1,4),(2,3),(3,2),(4,1)].

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) KVstore ktp vtp implements a 
simple key-value store, where ktp is the data type of the 
keys, and vtp is the data type of the values. 

Implement a module that implements the ADT hiding its 
internal structure from the user.

The following operations on the ADT must be implemented:
• empty :: Eq ktp => KVstore ktp vtp
  This creates an empty key-value store.
• insert :: Eq ktp => ktp -> vtp -> KVstore ktp vtp 
            -> KVstore ktp vtp 
  This inserts a key-value pair into the store (or over-
  writes an already existing pair with the given key).
• find :: Eq ktp => ktp -> KVstore ktp vtp -> Maybe vtp
  This looks up a value by its key.
• delete :: Eq ktp => ktp -> KVstore ktp vtp 
            -> KVstore ktp vtp
  This removes a key-value pair from the store.
• size :: Eq ktp => KVstore ktp vtp -> Int 
  This returns the number of key-value pairs in the store.

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

The function add is defined as follows:

  add :: Integer -> [Integer] -> [Integer]
  add a [] = []
  add a (x:xs) = (a + x):add a xs

Prove the following property:

  add a (add b xs) = add (a+b) xs 
  for all finite lists xs and for all values of a and b.

--------
Answer:







___________________________________________________________

8. Proof of property of foldr
___________________________________________________________

Prove for all finite lists xs::[a] and ys::[a], any 
value z::a, and any function f :: a -> a -> a that:

  foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs

--------
Answer:







___________________________________________________________