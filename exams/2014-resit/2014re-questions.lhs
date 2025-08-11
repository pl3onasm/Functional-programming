-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2014               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell function wtel?

  wtel [] = []
  wtel (x : xs) = if x == [] then wxs else x : wxs
    where wxs = wtel xs

--------
Answer: 



--------------------------------
Question 1.2:
What is the type of the following Haskell function cl?

cl ps = ps ++ [(p,s) | (p,q) <- ps, (r,s) <- ps, q == r]

--------
Answer:



--------------------------------
Question 1.3:
What is the type of the standard Haskell indexing 
operator !! (as an example [0..10]!!3 = 3)?

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following Haskell function map2?

  map2 f [] [] = []
  map2 f (x : xs) (y : ys) = (f x y) : map2 f xs ys

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function tw?

  tw = (\f -> (\x -> (f.f) x))

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Consider a positive integer N. We denote its decimal 
digits by X₀, X₁, ..., Xₖ. The number N is called a funny 
number if you can select at most three (but at least one) 
of its digits such that N is a divisor of the number
(X₀ + X₁ + ... + Xₖ - S)^S , where S is the sum of the 
selected digits. As an example, 1458 is a funny number 
since ((1 + 4 + 5 + 8) - (1 + 5))^{1+5} = 12^6 = 2985984 
is divisible by 1458. Note that we selected the two 
digits 1 and 5.

Write a Haskell function isFunny (including its type) that 
takes an integer number as its argument, and returns 
True if and only if this argument is a funny number.

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
Use a list comprehension to deﬁne a function 
inverse::[(a, b)]->[(b, a)] such that elem (x,y) ps if
and only if elem (y,x) (inverse ps).

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Use a list comprehension to make your own implementation 
of the standard Haskell function replicate. The call 
replicate n x yields a list of length n with x being the 
value of every element. So, replicate 5 'a' returns
"aaaaa".

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Deﬁne a function doubleReverse which takes a list of 
strings as its argument and reverses each element of the 
list and then reverses the resulting list. 
The implementation of doubleReverse must use a list 
comprehension. As an example: 

  doubleReverse ["hello", "world"] = ["dlrow", "olleh"]

--------
Answer:

>
>
>


___________________________________________________________

4. Infinite lists
___________________________________________________________

Question 4.1:
The function powers n returns the inﬁnite list 
[n^0, n^1, n^2, n^3, ...]. Give a recursive Haskell 
implementation (including its type) of the function powers.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
The sequence ak is deﬁned as follows:

  a₀ = 1 
  a₁ = 2
  aₖ = 3aₖ₋₁ + 2aₖ₋₂ for integer k > 1 
  
Deﬁne the inﬁnite list seqa, which is the list 
[a₀, a₁, a₂, a₃, a₄, ...], so seqa!!k should yield aₖ.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
In the figure included in this exam folder you see the 
first 5 rows of Pascal's triangle (see file Pascal.png)
To build the triangle, we start with the row [1] at the top
(we call this row 0), then continue placing numbers below 
it in a triangular pattern. Each row consists of elements 
that are the sum of the two numbers above it (except for 
the boundaries, which are all 1). In the ﬁgure, it is 
highlighted that the 4 in row 4 is obtained by adding the 
numbers 1 and 3 from row 3.

Give a deﬁnition of the infinite list 
pascalTriangle ::[[Integer]], such that pascalTriangle!!n
yields the nth row of Pascal's triangle 
So:   pascalTriangle!!4 = [1,4,6,4,1]

--------
Answer:

>
>
>


___________________________________________________________

5. ADT module
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

7. Proof by induction
___________________________________________________________

Given are the following Haskell deﬁnitions of the 
functions f and g:

  f :: Integer -> Integer
  f 0 = 0
  f 1 = 1
  f n = 5*(f (n-1)) - 6*(f (n-2))

  g :: Integer -> Integer -> Integer
  g n 0 = 1
  g n e = n*(g n (e - 1))

Prove for all natural numbers n: 

  f n = g 3 n - g 2 n

--------
Answer:







___________________________________________________________

8. Proof on lists
___________________________________________________________

Given are the deﬁnitions of the Haskell functions sum, 
and reverse:

  sum :: [Integer] -> Integer
  sum [] = 0
  sum (x:xs) = (sum xs) + x

  reverse :: [a] -> [a]
  reverse [] = []
  reverse (x:xs) = reverse xs ++ [x]

Prove the following property p: 

  sum (reverse xs) = sum xs for all ﬁnite lists xs.

--------
Answer:







___________________________________________________________