-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2015               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell expression?
(1,'2',"3")

--------
Answer: 



--------------------------------
Question 1.2:
What is the most general type of the below Haskell 
function f? 

f = map not 

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the function g?

g = (\ (a,b) -> a + b)

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the standard Haskell operator (.) 
that is used for function composition?

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function h? 

h = head.(  : ['a']) 

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Write a Haskell function middleElement (including its type) 
that computes the middle element of a list. The function 
should return Nothing in case there is no middle element, 
and Just m in case m is the middle element, so

• middleElement [1,2,3] should return Just 2
• middleElement [1,2] should return Nothing

You are not allowed to use the function length 
(nor are you allowed to implement it yourself).

[Hint: A possible solution uses a recursive helper 
function mid xs ys, of which the ﬁrst argument 'shrinks' 
faster than the second argument during the recursion. 
Deﬁne middleElement xs = mid xs xs where mid = .... ]

--------
Answer:

> 
> 
> 
> 
>
>



___________________________________________________________

3. Programming using list comprehensions and 
   Higher-order functions
___________________________________________________________

Counting sort is a well-known algorithm for sorting a list 
of small integers. In this problem, you may assume that the 
input is a list of digits (i.e. [0..9]). The algorithm 
consists of two passes. In the ﬁrst part, a histogram of 
the input is computed: for each possible value, the number 
of occurrences of this value is computed. In the second 
pass, using the histogram from the ﬁrst pass, the sorted 
output is produced.

Give an implementation of counting sort that uses solely 
list comprehensions and higher order functions. The use of
recursion is not allowed.

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Write a function palindromes (including its type) that 
accepts as its input a list of strings, and returns a list 
of all strings which are palindromes that can be 
constructed by concatenating two strings from the input 
list.
For example, palindromes ["a", "ab", "abb", "ac", "ca"] 
should return a list containing the strings (in any order):
"aa","aca","aba","abba","aca","acca", and "caac" 

The implementation must be a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Give a deﬁnition of the list fun42 which contains 42 
elements. These elements are functions of the type 
Integer -> Integer. The ﬁrst element is the function that 
adds 0 to its argument, the second adds 1 to its argument, 
and so on: the ith element adds i - 1 to its argument. 
The deﬁnition of fun42 must be a list comprehension.
For example, (fun42!!5) 10 should return 15.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Write a function triples (including its type) that accepts 
as its input an integer n, and returns the lexico-
graphically sorted list of all triples (a,b,c) such that 
a + b + c = n and 0 ≤ a < b < c. The implementation must 
be a list comprehension.
Example: 
          triples 8 
        = [(0,1,7),(0,2,6),(0,3,5),(1,2,5),(1,3,4)]

[Note: you can earn 3 points for a correct implementation, 
or 4 points for an efﬁcient correct implementation]

--------
Answer:

>
>
>


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a deﬁnition of the inﬁnite list 
[[1], [1,2], [1,2,3], [1,2,3,4], ....]

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Deﬁne the inﬁnite list fibs, which is the inﬁnite list of 
ﬁbonacci numbers. Recall that: 

  fib(0) = 0 
  fib(1) = 1
  fib(n) = f ib(n-1)+f ib(n-2) for n > 1 
  
So, take 10 fibs should return [0,1,1,2,3,5,8,13,21,34].

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Deﬁne the inﬁnite list abc, which is the inﬁnite list of
all non-empty strings over the alphabet {'a','b','c'}. 
This list needs to be sorted based on the length of the 
strings. Moreover, strings of equal length should be sorted
lexicographically (dictionary order). 

Example: 
            take 15 abc 
          = ["a","b","c","aa","ab","ac",
             "ba","bb","bc","ca","cb",
             "cc","aaa","aab", "aac"]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) PQ tp implements a simple 
data type for the storage of elements of the type tp.
The name PQ stands for Priority Queue. Elements can be 
inserted in such a queue in arbitrary order. However, 
retrieving an element from a non-empty priority queue 
always yields the smallest element.

Implement a module PQ such that the concrete  
implementation of the type PQ is hidden from the user.

The following operations on the data type PQ must be 
implemented:
• empty: returns an empty priority queue.
• isEmpty: returns True for an empty priority queue, 
  otherwise False.
• insert: returns the queue that is the 
  result of inserting an element.
• getmin: returns the 'smallest' element of the queue.
• remove: returns the queue that is obtained by 
  removing the smallest element.

--------

Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Given are the deﬁnitions of the functions take, and drop:

  take :: Int -> [a] -> [a]
  take 0 xs = []
  take n [] = []
  take n (x:xs) = x:take (n-1) xs

  drop :: Int -> [a] -> [a]
  drop 0 xs = xs
  drop n [] = []
  drop n (x:xs) = drop (n-1) xs

Prove the following property p: 

  take n xs ++ drop n xs = xs 
  for all ﬁnite lists xs and n ≥ 0

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  Given are the functions inorder and mirror:

  mirror :: BinTree a -> BinTree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  inorder :: BinTree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

Prove for all ﬁnite trees t: 
  
  reverse(inorder(mirror t)) = inorder t

--------
Answer:







___________________________________________________________