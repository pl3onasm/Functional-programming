-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2018               
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

  ("x", 'x', [True]) : []

--------
Answer: 


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

  (+1) . (0<)

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

  (+1) . (0+)

--------
Answer: 



--------------------------------
Question 1.4:
Is the following definition of f type correct?
If YES, then give the most general type of f.

  f = [] : [[xs] | xs <- f]

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following function g?

  (.).(.)

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

We can represent a directed graph by a list (with the type 
[(Int,Int)]) of arcs.

For example, the graph depicted in the file graph.png 
included in this exam folder can be represented by the 
list [(1,2),(1,3),(3,2),(3,4),(4,3)].

Write a function 
  paths :: Int -> Int -> [(Int,Int)] -> [[Int]] 
such that paths a b arcs returns a list containing all the 
paths from node a to node b using steps taken from the list
arcs. Note that these paths may not use an arc more than 
once. 

For example: 

    paths 1 2 [(1,2),(1,3),(3,2),(3,4),(4,3)]
  = [[1,2],[1,3,2],[1,3,4,3,2]]

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
Give a Haskell implementation (including its type) of the 
function mapEach such that mapEach f xss returns a list of 
lists, containing the result of applying f to each element 
of each list in xss.

For example: 

    mapEach (+ 2) [[5, 4, 1], [7, 6], []] 
  = [[7, 6, 3], [9, 8], []]

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Given is the function gcd that returns the greatest common 
divisor of its two arguments (i.e. gcd 36 42 returns 6):

> gcd :: Int -> Int -> Int
> gcd a b = if b == 0 then a else gcd b (a `mod` b)

Implement the function listgcd that takes a list of 
integers, and returns the greatest common divisor of all 
elements in the list. Your implementation must make use of 
the function foldr. 

For example:  listgcd [25,15,125,555] = 5

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Consider the following haskell definition of the function 
scanl:

  scanl f z xs = 
    [foldr f z (take len xs) | len <- [0..length xs]]

For example, scanl (+) 1 [1..10] returns 
[1,2,4,7,11,16,22,29,37,46,56]. The above implementation 
of scanl is quite inefficient (it has quadratic time 
complexity). Give an equivalent implementation (including
its type) that runs in linear time.

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
filter (including its type) using a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Write a function sumdiv3not5 (including its type) that 
takes a list of Integers and returns the sum of the squares
of those numbers in the list that are divisible by 3 but 
not by 5. 

  For example: sumdiv3not5 [-6,15,2,3] = 45

You must use a list comprehension, and are not allowed 
to use recursion.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Give an implementation of the standard Haskell function 
zipWith (including its type) using a list comprehension.

--------
Answer:

>
>
>



___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a definition of the infinite list inits (including its 
type) of which the n-th element is a list containing the 
numbers 0,1,2,..,n. 

Example:    take 5 inits 
          = [[0],[0,1],[0,1,2],[0,1,2,3],[0,1,2,3,4]]

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Give a definition of the function powerfunc (including its 
type) that accepts a function f on its input, and outputs
the infinite list of repeated applications of f. The first 
element (index 0) of this list should be the function f 
raised to the power zero, i.e. the identity function. 
The second element (index 1) is the function f raised to 
the power one, i.e. f itself. The third element (index 2) 
is the function f raised to the power two, i.e. f(f(x)) 
for all x, and so on.

For example:  

    head ((drop 5) (powerfunc (+2))) 0 = 10

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Consider the following Haskell code:

  f a b = a : b

  g a [] = a : []
  g a b = a : b

  list1 = foldr f [] [1..]
  list2 = foldr g [] [1..]

What will happen if we try to compute take 10 list1? 
And what happens if we try to compute take 10 list2?
Explain your answers.

--------
Answer:




___________________________________________________________

6. ADT module
___________________________________________________________

The unary numeral system is the simplest numeral system to 
represent natural numbers. To represent the natural number 
N, an arbitrarily chosen symbol representing one is 
repeated N times. For example, the number 5 can be 
represented by the list [1, 1, 1, 1, 1] (here, the 
arbitrary chosen symbol is the digit 1). Hence, in this 
notation, the length of the list is the actual value it 
represents. We represent the value zero by the empty list.

The type NatNum is an Abstract Data Type (ADT) for 
implementing natural numbers. Its implementation uses the 
unary numeral system. Implement a module NatNum such that 
the concrete implementation of the type NatNum is hidden 
to the user.

The following operations on natural numbers need to be 
implemented:

• integerToNat n converts the Integer n into 
  the NatNum that represents n.
• natToInteger n converts the natural number n 
  into its decimal Integer value.
• isZero n returns True if and only if the 
  natural number n represents 0.
• isLessThan a b: returns True if and only if the
  natural number a is less than the natural number b.
• plus a b: returns the natural number that is 
  obtained by adding the natural numbers a and b.
• mul a b: returns the natural number that is 
  obtained by multiplying the natural numbers a and b.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof on foldr and foldl
___________________________________________________________

Consider the following Haskell definitions of the functions
foldr and foldl:

  foldr f z [] = z
  foldr f z (x:xs) = f x (foldr f z xs)

  foldl f z [] = z
  foldl f z (x:xs) = foldl f (f z x) xs

Prove the following property p: 

  foldr (+) z xs = foldl (+) z xs 
  for all finite lists xs

--------
Answer:







___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree, and the functions mirror 
and inorder:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

Prove for all finite trees t: 

  inorder(mirror t) = reverse(inorder t)

You may use, without a proof, that the operator ++ is 
associative, i.e. xs++ys++zs = (xs++ys)++zs = xs++(ys++zs).

--------
Answer:







___________________________________________________________