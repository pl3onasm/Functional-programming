-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2017               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the most general type of the following expression? 

  [(('A', "BC"), [True])]

--------
Answer: 



--------------------------------
Question 1.2:
What is the most general type of the following expression?

  [id, abs]

--------
Answer:



--------------------------------
Question 1.3:
What is the most general type of the function f?

  f g (x,y) = g x y

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the function map?

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

  h f g x y = f (g x y) x

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

The Luhn algorithm is a simple checksum formula used to 
validate a credit card number. It works as follows.
Let n be the number to be checked, e.g. n = 49927398716. 

The first step is to double every second digit, starting 
from the right (so, in the example we only consider the 
digits .9.2.3.8.1.). If doubling a digit results in a 
number greater than 9 then subtract 9 from the number. 
For this example, the result of this first step is the 
number x = 49947697726. 

In the second step of the algorithm, all digits of x are 
summed up. For the example, we get:
4 + 9 + 9 + 4 + 7 + 6 + 9 + 7 + 7 + 2 + 6 = 7
If this sum is evenly divisible by 10, then the number is 
a valid credit card number, otherwise it is invalid. The 
conclusion is that the example number is a valid credit 
card number.

Write a Haskell function isValidNumber :: Integer -> Bool 
that returns True if and only if its first argument is a 
valid credit card number. 

Example:    isValidNumber 49927398716 = True

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
Using the higher-order functions foldr and map, define a 
function powersum (including its type) which takes two
non-negative integers n and e. It returns the sum of the 
first n positive integers raised to the power e.

So: powersum 4 3 = 1^3 + 2^3 + 3^3 + 4^3 
                 = 1 + 8 + 27 + 64 
                 = 100

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Define the function filter (including its type) using the 
function foldr.

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Using function composition (.), foldr, map and the identity 
function id, write a function pipeline (including its type)
which given a list of functions, each of type a -> a, will 
form a pipeline function of type [a] -> [a].
In such a pipeline, each function in the original function 
list is applied in turn to each element of the input 
(assume the functions are applied from right to left in 
this case). 

For example:   pipeline [(+1),(*2)] [1,2,3] = [3,5,7]

--------
Answer:

>
>
>



___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Give an implementation of the standard Haskell function 
concat (including its type) as a list comprehension. 
Recall that: 
  concat [[1,2],[],[3]] = [1,2,3] 
  concat ["hello", "world"] = "helloworld"

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Show how the single comprehension 
  [(x,y) | x <- [0..m], y <- [0..n]] 
  
with two generators can be re-expressed using two 
comprehensions with single generators. 
[Hint: make use of the library function concat.]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
The dot product of two vectors 
a = [a₀, a₁, ..., aₙ] 
b = [b₀, b₁, ..., bₙ]

is defined as:

  a . b = ∑_{i=1}^n (aᵢ * bᵢ) =
          a₀*b₀ + a₁*b₁ + ... + aₙ*bₙ  

Implement the haskell function dotProduct using a list 
comprehension. It takes two lists as input, and returns
their dot product as output. You may assume that the 
input lists have the same length.

--------
Answer:

>
>
>



___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a recursive expression for the list pairs which is the
infinite list of pairs (n,n+1), where n ranges over the
natural numbers. For example:

  take 3 pairs = [(0,1),(1,2),(2,3)]

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Give a definition of the function gendups which takes a 
(possibly infinite) list of positive Integers and generates 
the (posssibly infinite) list where each element x of the 
input list has been replaced by x copies of itself. 

For example:
  gendups [1,2,1,3] = [1,2,2,1,3,3,3]
  gendups [1..] = [1,2,2,3,3,3,4,4,4,4,....]

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Define the function sums::[Integer] -> [Integer], that 
takes an infinite list of Integers and produces the 
corresponding infinite list of prefix sums.

For example: sums [0,2..] should produce the infinite list 
             [0, 0+2, 0+2+4, ...] = [0, 2, 6, ...]

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The type Stack a is an Abstract Data Type (ADT) for stacks 
containing elements of the type a. Recall that a stack is 
a container that works according the LIFO (Last In First 
Out) principle. In other words, the element that was most
recently inserted by a push operation is returned by a top 
operation.

Implement a module Stack such that the concrete 
implementation of the type Stack is hidden to the user. 
You may choose yourself a suitable data representation for 
stacks.

The following operations on stacks need to be implemented:
• empty returns an empty stack.
• isEmpty returns True for an empty stack, 
  otherwise False.
• push: returns the stack that is obtained by 
  adding an element to the stack.
• pop: returns the stack that is obtained by 
  removing the top element from the stack.
• top: returns the element that was most 
  recently added to the stack.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Prove the following property p: 

  map (f.g) xs = (map f . map g) xs 
  for all finite lists xs

[Note: refer to the file functions.md for the 
 definitions of map and (.)]

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and mapTree:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f Empty = Empty
  mapTree f (Node x t1 t2) = 
      Node (f x) (mapTree f t1) (mapTree f t2)

Prove for all finite trees t: 

  inorder (mapTree f t) = map f (inorder t)

Associativity of (++) may be used without proof:

    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

[Note: refer to the file functions.md for
 definitions of map and (++)]

--------
Answer:







___________________________________________________________