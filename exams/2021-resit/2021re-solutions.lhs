-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2021               
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

[[],[[]]]

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[not, id]

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

[(+), (:)]

--------
Answer: 



--------------------------------
Question 1.4:
What is the most general type of the following function f?

f = foldr (&&)

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function g?

g = map map

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Consider the following iterative process. Given two 
positive integers n, and g (where 0 < n ≤ g), we try to 
reach g starting from n, where we are allowed to 
iteratively use the operations n → 3 x n, n → 2 x n, and 
n → n + 1. 

For example, let n = 1, and g = 42. We can reach 42 in 5 
steps in the following way:

1 → (+1) → 2 → (x3) → 6 → (+1) → 7 → (x3) → 21 → (x2) → 42

Clearly, several other possibilities exist to reach 42, 
for example:
    1 → (+1) → 2 → (+1) → 3 → (x2) → 6 
    → (+1) → 7 → (x3) → 21 → (x2) → 42 

However, there is no way to reach 42 (starting in 1) with 
fewer than 5 steps.

Write a Haskell function minSteps :: Int -> Int -> Int such
that the call minSteps n g returns the minimum number of 
computational steps to get from n to g. So, minSteps 1 42 
returns 5.

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
What will be the output if you enter the expression 
  foldr (++) [0] (map (\x -> [x]) [1..10]) 
in the Haskell interpreter?

--------
Answer:



--------------------------------
Question 3.2:
Give an implementation of the function reverse that 
makes use of foldr.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Give an implementation (and the most general type) of the 
function zip that makes use of the function zipWith.

--------
Answer:

>
>
>


--------------------------------
Question 3.4:
Give an implementation of the operator ++ that makes use 
of foldr.

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What will be the output if you enter the following
expression in the Haskell interpreter? 

  [(x,y,x+y) | x<-[0,1,2], y<-[3,4]] 

--------
Answer:




--------------------------------
Question 4.2:
The function heads takes a list of lists and returns a list 
containing the heads of those lists.
For example: 

  heads[[1,2,3],[4,5],[],[6,7,8]] 
  should yield: [1,4,6]

Give an implementation of heads (and it most general type) 
that makes use of a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Implement the function zipWith using a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.4:
Use a list comprehension and the function zip to write a 
Haskell function locations n xs that returns the list of
all indexes i such that the ith element of xs is n 
(i.e. xs!!i == n). Note that the first element of a list 
has index 0.

For example: 
  locations 0 [x `mod` 10 | x <- [1..50]] 
  should yield: [9,19,29,39,49] 
  
You are not allowed to use the indexing operator !! in the
implementation.

--------
Answer:

>
>
>


--------------------------------
Question 4.5:
Deﬁne a function doubleReverse which takes a list of lists 
as its argument and reverses each element of the list and 
then reverses the resulting list. The implementation of 
doubleReverse must use a list comprehension. 
As an example: 

  doubleReverse ["palindrome", "word"] 
  = ["drow", "emordnilap"]
  
--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a Haskell expression that produces the inﬁnite string 
"abbaaabbbbaaaaabbbbbbaaaaaaa...", i.e one a, two bs, three 
as, four bs, etc.

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Give a definition of the infinite list factorials of 
factorial numbers.
For example:
    take 10 factorials 
    should yield: [1,1,2,6,24,120,720,5040,40320,362880]

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Give a deﬁnition of the inﬁnite list bitpals of non-empty 
palindromic strings that consist of the letters 'a' and
'b'. For example: 
  take 8 bitpals 
  may return: ["a","b","aa","bb","aaa","bab","aba","bbb"]

The list bitpals must be organized such that a test like 
elem "abba" bitpals terminates.

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

Implement a module Set that exports the ADT Set. In the 
concrete implementation you must make use of an ordered 
list of set elements, however this concrete implementation 
must be hidden from the user of this module.

The following operations on the data type Set must be 
implemented:
• empty returns an empty set.
• isEmpty returns True for an empty set, otherwise False.
• isElement x returns True if x is a member of the set, 
  otherwise False.
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

In this problem we use the following deﬁnition of sum (the 
deﬁnitions of reverse and ++ are in the file functions.md):

  sum [] = 0
  sum (x:xs) = x + sum xs

Prove:   sum xs = sum(reverse xs) for any ﬁnite list xs

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions mirror, and 
size:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  size :: Tree a -> Integer
  size Empty = 0
  size (Node x l r) = 1 + size l + size r

Prove for all ﬁnite trees t: 

  size (mirror t) = size t

--------
Answer:







___________________________________________________________