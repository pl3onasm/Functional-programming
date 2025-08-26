-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2021               
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

  [[1<2, [not False], 2>1]]

--------
Answer: 



--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

  [[not]]

--------
Answer:



--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

  [(&&), (||), not]

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following function g?

  g = (:).(:)

--------
Answer: 



--------------------------------
Question 1.5:
What is the most general type of the following function f?

  f = \x y -> x (x (x y))

--------
Answer: 



___________________________________________________________

2. Programming in Haskell
___________________________________________________________

In number theory, the Chinese remainder theorem states that
if one knows the remainders of division of an integer x by
several integers, then one can determine uniquely the 
remainder of the division of x by the product of these 
integers, under the condition that the divisors are pair-
wise coprime (i.e. their greatest common divisor is 1).

For example, we want to find the smallest integer x such 
that x mod 6 = 2, x mod 5 = 3, and x mod 7 = 5.
Clearly, the divisors 6, 5, and 7 are pairwise coprime so 
the theorem guarantees that x exists. The algorithm to find
x works as follows. 

We start with the first equation being x mod 6 = 2. This 
means that candidates for x are in the list [2,8,14,20,...] 
The smallest candidate from this list that satisfies the 
second equation x mod 5 = 3 is 8. Hence, the solution must 
be of the form x = 8 + k · 6 · 5 = 8 + k · 30, where k is a 
non-negative integer. Of course, the factor 30 is obtained 
by multiplying the divisors 6 and 5. Candidates that 
satisfy this equation are in the list [8,38,68,98,....].
The smallest candidate from this list that also satisfies 
x mod 7 = 5 is 68, which is the final solution.

Write a function crt::[(Integer,Integer)] -> Maybe Integer 
that accepts a non-empty list of pairs (aᵢ, dᵢ), which 
should be interpreted as x mod dᵢ = aᵢ. You may assume 
that 0 ≤ aᵢ < dᵢ for all i. The function returns Nothing if
there exists a pair of divisors which are not coprime. 
Otherwise, it should return Just x, where x is the smallest 
non-negative integer that satisfies all equations. 
Your program must use the algorithm described above (and 
not use any other technique). So, crt [(2,6),(3,5),(5,7)] 
should return Just 68, while crt [(0,2),(1,4)] should 
return Nothing.

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
Write a function cntsat (including its most general type) 
which takes a predicate function p and a finite list xs and
returns the number of elements x from xs that satisfy p x. 
You are not allowed to use a list comprehension.

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Give an implementation (and the most general type) of the 
function filter2 which takes a predicate and a list of
lists, and outputs the list of lists that is obtained by 
filtering each list separately.

For example: 
  
  filter2 even [[1,2,3,4],[3],[4,5],[]] 
  should return: [[2,4],[],[4],[]]

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Give a Haskell definition of the function applyAll 
(including its type) which takes a list of functions 
and an argument. 
It returns the value that is obtained by successively 
applying the functions to this argument (i.e. function
composition).

For example, 

  applyAll [(+1), (*2), (\x->x-2)] 3 
  should return: 6 (because 6=((3+1)*2)-2).

--------
Answer:

>
>
>


--------------------------------
Question 3.4:
The standard Haskell function foldl is similar to foldr
except that parentheses associate to the left. For example,
foldl (+) 0 [x,y,z] = (((0+x)+y)+z). Give a Haskell 
implementation of the function foldl (including its most 
general type).

--------
Answer:

>
>
>


--------------------------------
Question 3.5:
The function digitsToInteger :: [Integer] -> Integer 
converts a list of digits into the corresponding integer. 
For example, digitsToInteger [4,2] should return 42.
Using the function foldl from the previous exercise, 
implement the function digitsToInteger. Even if you were 
not able to answer the previous question, you may still 
assume that foldl is available.

--------
Answer:

>
>
>


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What will be the output if you enter the expression 
[x + y | x<-[1..3], y<-[0..2]] in the Haskell interpreter?

--------
Answer:




--------------------------------
Question 4.2:
The function mapfilter is defined as: 

  mapfilter f p = (map f).(filter p) 
  
Give an alternative implementation (without using map or 
filter) of this function using a list comprehension. Also 
give the most general type of this function.

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Write a function factors :: Integer -> [Integer] which 
returns all factors of its argument. Next, write the 
function perfect :: Integer -> [Integer] such that 
perfect n returns the list of all perfect numbers in 
the domain [2..n]. 
Recall that a number is called a perfect number if it 
equals the sum of its divisors (excluding itself). The 
implementation of both functions must make use of a list 
comprehension.

For example:

  factors 6 should return [1,2,3,6]
  
  perfect 1000 should return [6,28,496].

--------
Answer:

>
>
>


--------------------------------
Question 4.4:
Make use of a list comprehension to implement the function 
subs which takes a non-empty list xs and produces the
list of all non-empty subsequences of xs. 
Note that if xs contains duplicates, then the output also 
contains duplicates. Also, give the type of the function 
subs. The order of the elements in the output of subs is 
not important. 

For example, subs [1,2,3,2] may return:

  [[1],[1,2],[1,2,3],[1,2,3,2],[2],
  [2,3],[2,3,2],[3],[3,2],[2]]

--------
Answer:

>
>
>


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a Haskell expression that yields the infinite list 
[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,.....].

--------
Answer:

> 
>
>


--------------------------------
Question 5.2:
Given is the infinite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

Use it to define the Boolean function 
semiprime :: Integer -> Bool which returns True if and only 
if its argument is a product of exactly two prime numbers.

--------
Answer:

> 
>
>


--------------------------------
Question 5.3:
Given is the following definition of the infinite list fs:

  fs = genfs 0 1 
    where genfs a b = a : genfs b (2*a + 3*b)

Use zip or zipWith to give an equivalent definition of the 
list fs.

--------
Answer:

>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

ADT module (15 points) The type Array a is an Abstract Data 
Type (ADT) for arrays of type a. Recall that an array is a
linear data structure that allows indexing. In this problem, 
we assume that indexing starts from 0. The arrays in this 
problem are such that indexing an uninitialized array 
location yields Nothing, otherwise it returns Just x where x 
is the indexed value. Also, indexing out of bounds returns 
Nothing. 

Implement a module Array which exports the abstract data 
type but hides the concrete implementation. You may choose 
yourself a suitable data representation for arrays. 

The following operations on arrays need to be implemented:
• create n: returns an uninitialized array of length n.
• setElement arr idx e: returns the array that is 
  obtained by overwriting location idx of arr with e.
• getElement arr idx: returns the element at index idx 
  in the array arr.
• resize arr len: returns a resized array that is 
  constructed from arr such that it contains len elements. 
  If this means that the array is expanded, then 
  uninitialised values are appended to the array. 
  If this means that the array gets shorter, then the 
  elements with index len and higher are discarded.
• size: returns the length of the array.
• elems: returns the number of initialized locations 
  of the array.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof on lists
___________________________________________________________

Given are the following definitions of the functions take 
and drop:

  take _ [] = []
  take n (x:xs) = if n <= 0 then [] else x:take (n-1) xs

  drop _ [] = []
  drop n (x:xs) = if n <= 0 then (x:xs) else drop (n-1) xs

Prove the following property:

  take n xs ++ drop n xs == xs for any integer n 
  and any finite list xs.

[Note: the definition for ++ is given in functions.md]

--------
Answer:







___________________________________________________________

8. Proof on data structures
___________________________________________________________

Given is the data type Expr and the functions eval, 
and isZero:

  data Expr = Value Integer | Add Expr Expr | Mul Expr Expr

  eval (Value n) = n
  eval (Add a b) = eval a + eval b
  eval (Mul a b) = eval a * eval b

  isZero (Value n) = n==0
  isZero (Add a b) = isZero a && isZero b
  isZero (Mul a b) = isZero a || isZero b

Prove for all finite expressions e: 

  isZero e ⇒ eval e == 0

--------
Answer:







___________________________________________________________