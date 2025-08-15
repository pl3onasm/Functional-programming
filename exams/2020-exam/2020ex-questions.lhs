-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2020               
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

[not, []]

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[[not],[]]

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

(&&).(&&)

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following function g?

g = not.not

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function f?

f = \x -> \y -> \z -> (x (x y), x z)

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Consider the following two lists: 

  [[1,2,3],[4,2],[]]  
  
  [[2,4],[],[2,3,1]] 
  
If we ignore the order of elements in lists, then these 
lists are equal.

Implement a Haskell function 

compareListOfLists :: Eq a => [[a]] -> [[a]] -> Bool 

such that the call compareListOfLists xss yss returns 
True if and only if xss and yss are equal if we ignore 
the order of elements in lists.

--------
Answer:

> compareListOfLists :: Eq a => [[a]] -> [[a]] -> Bool 
> 
> 
> 
>
>



___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function flip such that (flip f) a b returns f b a. 
The implementation must make use of a lambda expression. 
Also, give the type of the function flip.

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Without using recursion or a list comprehension, write a 
function evenLists which takes a list of lists of Integers
as its argument and removes from it every list not 
containing an even numer. Also, give the type of this 
function.
For example, evenLists [[1,2],[7,5,11],[1,3],[21,2,42]] 
should return [[1,2],[21,2,42]].

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Implement the funtion append such that append xs ys returns
xs++ys. You must make use of the standard function foldr, 
and are not allowed to use the ++ operator itself.

--------
Answer:

>
>
>


--------------------------------
Question 3.4:
Without using recursion or a list comprehension, implement 
the funtion pals that takes a list of lists as its argument
and removes from it every list that is not a palindrome. 
Also, give the most general type of this function.
For example, pals ["madam","pop","your","stack"] 
should return ["madam","pop"].

--------
Answer:

>
>
>


--------------------------------
Question 3.5:
Implement the function zip using zipWith.

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function replicate using a list 
comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Use a list comprehension to implement the function pairs 
which takes a list xs ands returns a list of all pairs that
can be constructed from xs. 
For example, pairs [1,2,3] should return the following 
list (in this order):

[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
The function pairs2 also takes a list xs and outputs a list
of pairs. A recursive implementation is given below.
For example, pairs2 [1,2,3,4] returns 
  [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

pairs2 [] = []
pairs2 (x:xs) = p x xs ++ pairs2 xs
  where 
    p x [] = []
    p x (y:ys) = (x,y):p x ys

Give an equivalent implementation that makes use of (a) 
list comprehension(s) that replaces the recursions.

--------
Answer:

>
>
>


--------------------------------
Question 4.4:
The function perms takes a list of Ints and returns a list 
of all possible permutations of this list. 
For example, perms [1..3] should return (in this order): 

  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Implement perms using a list comprehension.

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
  
Write a function isPrime n that returns True only if n is 
a prime number.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Using zip or zipWith, give a deﬁnition of the inﬁnite list 
delayedFib which is the list of delayed Fibonacci numbers 
which are deﬁned as:

  F (n) = n for n < 3, 
  F (n) = F (n - 1) + F (n - 3) for n ≥ 3

So, the expression take 10 delayedFib 
equals [0,1,2,2,3,5,7,10,15,22].

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Implement the inﬁnite list abc which consists of all 
strings that can be produced with the letters 'a', 
'b', and 'c'. For example, take 25 abc should return:

  ["a","b","c","aa","ba","ca","ab","bb","cb","ac",
   "bc","cc","aaa","baa","caa", "aba","bba","cba",
   "aca","bca","cca","aab","bab","cab","abb"]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type RLElist is an Abstract Data Type (ADT) that is 
used to store lists that typically contain chunks of 
repeated values. A typical example of that would be a list 
like: 

  [1,1,1,4,5,2,2,2,2,2,2,1,1,1]
  
This list can be stored more compactly as a list of pairs, 
where the ﬁrst element represents a data element and the 
second contains the length of the chunk. This type of data 
storage is called RLE (Run Length Encoding). For the given 
example, this representation would be:

  [(1,3),(4,1),(5,1),(2,6),(1,3)]

Implement a module RLElist that exports the ADT RLElist but 
hides the implementation. The following operations need to 
be implemented:

• fromList xs returns the RLElist representation of 
  the standard list xs.
• toList xs converts the RLElist xs into a standard list.
• hd xs returns the head of the non empty RLElist xs.
• tl xs return the tail of the non empty RLElist xs.
• cons x xs returns the RLElist that is obtained by 
  placing the element x ahead of the RLElist xs.
• cat xs ys returns the RLElist that is obtained by 
  concatenating the RLElists xs and ys.
• len xs returns the length (the number of data items) 
  in the RLElist xs.
• rev xs returns the RLElist that is obtained by 
  reversing the data lements in the RLElist xs.

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell function:

  f [] ys = []
  f (x:xs) ys = ys ++ f xs ys

Prove that 

  length xs * length ys = length(f xs ys) 
  
for all ﬁnite lists xs and ys.

[Note: find the definition of length in the file
       functions.md included in this exam folder]

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and size:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ (x:inorder r)

  size :: Tree a -> Integer
  size Empty = 0
  size (Node x l r) = size l + 1 + size r

Prove for all finite trees t: 

  size(t) = length (inorder t)

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately. 
 The definition of length is given in functions.md]

--------
Answer:







___________________________________________________________