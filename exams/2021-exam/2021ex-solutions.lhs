> import Prelude hiding (foldl, gcd)

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

No, the expression is not type correct. In the inner list, 
the first and last elements are of type Bool, while the 
middle element is of type [Bool]. Since all elements of a 
list must have the same type, this mismatch makes the 
expression ill-typed.

--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[not]

--------
Answer:

Yes, this expression is type correct. It is a list 
containing the Boolean negation function. Its type is
[Bool -> Bool].

--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

[(&&), (||), not]

--------
Answer: 

No, the expression is not type correct. The first two
elements are binary Boolean functions, while the last
element is a unary Boolean function. As the types of
the elements are not the same, and cannot be unified
into a single type, the expression is ill-typed.

--------------------------------
Question 1.4:
What is the most general type of the function g?

g = (:).(:)

--------
Answer: 

This one is a bit tricky and can use some explanation. 
The (.) operator is the function composition operator, 
which takes two unary functions and returns their 
composition, also a unary function. 
It is this operator (.) that forces the inner (:) to be 
partially applied to an element x :: a, turning it from a 
binary function (:) :: a -> [a] -> [a] into a unary 
function (: x) :: [a] -> [a], which prepends the 
element x to a list of type [a]. 

The outer (:) then receives this function as its first
argument, and when partially applied, yields a unary 
function:

    (: f) :: [[a] -> [a]] -> [[a] -> [a]]

that prepends the received function f = (: x) to a list 
of such functions.

Since the composition operator (.) has the type 
(.) :: (b -> c) -> (a -> b) -> a -> c, we can deduce the
type of g by simply substituting the types of the
components:

    g :: a -> [[a] -> [a]] -> [[a] -> [a]]

Note that the function arrow -> is right-associative, 
so the above type is equivalent to

    g :: a -> ( [[a] -> [a]] -> [[a] -> [a]] )

This means that g takes a single element x :: a, and 
returns a unary function which takes a list of unary 
functions of type [a] -> [a], that outputs the same
list of unary functions with a new function (: x) 
prepended to it.

To clarify, we can define g as follows:

    g x ys = (: x) : ys

Suppose x is of type Int, say x = 5, and ys is a list of 
unary functions of type [Int] -> [Int], say 
ys = [tail, reverse]

Then we have:  g 5 ys = [(: 5), tail, reverse] 

The output is a list that contains functions that can be
applied to lists of Ints, where the first function
(: 5) prepends the integer 5 to any list of Ints.

--------------------------------
Question 1.5:
What is the most general type of the following function f?

f = \x y -> x (x (x y))

--------
Answer: 

Since x is applied to y, it must be a unary function.
So x :: a -> b. Next, we see that x is applied to its own 
output twice, so the earlier output of type b must be
of the same type as the input of x. This means that we 
now have x :: a -> a.
The function f is a lambda function that takes two
arguments: x of type a -> a and y of type a.
Thus, the most general type of f is:
  f :: (a -> a) -> a -> a


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

> gcd :: Integer -> Integer -> Integer
> gcd a 0 = abs a
> gcd a b = gcd b (a `mod` b)

> coprime :: (Integer, Integer) -> Bool
> coprime (a, b) = gcd a b == 1

> crt :: [(Integer, Integer)] -> Maybe Integer
> crt [] = Nothing
> crt prs@((a, d) : rest)
>   | all coprime [(x, y) | (_, x) <- prs, 
>                           (_, y) <- prs, x < y]
>     = Just (solve a d rest)
>   | otherwise = Nothing
>   where
>     solve x _ [] = x
>     solve x step ((a, d) : xs) =
>       let candidate = next x step a d
>       in solve candidate (step * d) xs
>     -- Finds the next valid candidate
>     next can step a d
>       | can `mod` d == a = can
>       | otherwise        = next (can + step) step a d

This implementation follows the problem statement literally 
but is not optimal in terms of efficiency. Currently, it 
finds the next valid candidate by linearly generating 
intermediate candidates in increments of step until the 
modulus condition (can `mod` d == a) is satisfied. 
For large step and d, however, this can be very 
inefficient.
A more efficient alternative would compute the next valid
candidate directly using modular arithmetic (modular 
inverses) instead of a brute-force linear search.

___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function cntsat (including its most general type) 
which takes a predicate function p and a ﬁnite list xs and
returns the number of elements x from xs that satisfy p x. 
You are not allowed to use a list comprehension.

--------
Answer:

> cntsat :: (a -> Bool) -> [a] -> Int
> cntsat p = length . filter p 

First we use filter to select all elements satisfying the
predicate p from the input list. Then we apply the length
function to count the number of elements in the filtered 
list. 

--------------------------------
Question 3.2:
Give an implementation (and the most general type) of the 
function filter2 which takes a predicate and a list of
lists, and outputs the list of lists that is obtained by 
ﬁltering each list separately.

For example: 
  
  filter2 even [[1,2,3,4],[3],[4,5],[]] 
  should return: [[2,4],[],[4],[]]

--------
Answer:

> filter2 :: (a -> Bool) -> [[a]] -> [[a]]
> filter2 p = map (filter p)

We use the map to apply the filter function to each 
sublist in the input list of lists. The filter function 
selects only those elements that satisfy the predicate p, 
resulting in a new list of lists where each sublist 
contains only the elements that satisfy the predicate.

--------------------------------
Question 3.3:
Give a Haskell deﬁnition of the function applyAll 
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

> applyAll :: [a -> a] -> a -> a
> applyAll fs x = foldl (\acc f -> f acc) x fs

We can use foldl to apply the functions from left to right
to the input argument. For each function f in the list fs, 
we apply it to the accumulated value acc, starting with the
initial value x. The result is the final value after all
functions have been applied. 
Note that we need to use foldl in this version because we 
want to apply the functions in the order they appear in the 
list, which is left-to-right. Using foldr would apply them
right-to-left, which would yield a different result.

However, foldl is not included in the list of freely 
available functions (functions.md), while foldr is.
Therefore, we use foldr in combination with reverse to 
achieve the same effect:

> applyAll' :: [a -> a] -> a -> a
> applyAll' fs x = foldr (\f acc -> f acc) x (reverse fs)

--------------------------------
Question 3.4:
The standard Haskell function foldl is similar to foldr
except that parentheses associate to the left. For example,
foldl (+) 0 [x,y,z] = (((0+x)+y)+z). Give a Haskell 
implementation of the function foldl (including its most 
general type).

--------
Answer:

> foldl :: (b -> a -> b) -> b -> [a] -> b
> foldl f acc []       = acc
> foldl f acc (x : xs) = foldl f (f acc x) xs 

This implementation of foldl takes a binary function f,
an initial accumulator acc, and a list xs. It executes the
desired left-associative computation of the form 
(((n `f` x) `f` y) `f` z) by immediately applying the 
function f to the current accumulator and the head of the 
list, and then recursively calling foldl on the rest of 
the list with the updated accumulator (f acc x). 
The base case is when the list is empty, in which case 
it simply returns the accumulator.

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

> digitsToInteger :: [Integer] -> Integer
> digitsToInteger = foldl (\acc d -> acc * 10 + d) 0

We use foldl to iterate over the list of digits, starting
with an initial accumulator of 0. For each digit d, we
multiply the current accumulator acc by 10 (to shift the
previous digits by one place to the left) and then add the
current digit d to it. This effectively builds the integer
from the list of digits in a left-to-right manner, where
the first digit becomes the most significant digit in the
resulting integer.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What will be the output if you enter the expression 
[x + y | x<-[1..3], y<-[0..2]] in the Haskell interpreter?

--------
Answer:

The outcome will be a list with the pairwise sums:
  [1+0, 1+1, 1+2, 2+0, 2+1, 2+2, 3+0, 3+1, 3+2]
= [1, 2, 3, 2, 3, 4, 3, 4, 5]

A list comprehension produces the sums in nested loop 
order, where for each x in the first list, it iterates 
over all y in the second list, producing all combinations 
of sums x + y. The output is a flat list of these sums.

--------------------------------
Question 4.2:
The function mapfilter is deﬁned as: 

  mapfilter f p = (map f).(filter p) 
  
Give an alternative implementation (without using map or 
filter) of this function using a list comprehension. Also 
give the most general type of this function.

--------
Answer:

> mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b] 
> mapfilter f p xs = [f x | x <- xs, p x]

For each x drawn from xs, if p x is true, we include f x 
in the output list. This exactly mimics (map f).(filter p) 
but uses only a list comprehension instead.

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

> perfect :: Integer -> [Integer]
> perfect n = [x | x <- [2..n], x == sum(factors x) - x]

> factors :: Integer -> [Integer]
> factors n = [x | x <- [1..n], n `mod` x == 0]

The function factors returns all integers from 1 to n that 
divide n evenly. In turn, the function perfect uses this
to filter numbers from 2 to n that are perfect by checking 
whether the number equals the sum of its factors excluding 
itself (i.e., sum of all factors minus the number itself).

A more efficient implementation of factors would use the
fact that factors come in pairs, but the above implemen-
tation is straightforward and the question does not
require optimization.

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

> subs :: [a] -> [[a]]
> subs [] = []
> subs (x:xs) = [[x]] ++ [x:ys | ys <- subs xs] ++ subs xs

The base case is when the input list is empty, in which 
case it returns an empty list.
For a non-empty list, it includes subsequences with just 
[x], subsequences starting with x followed by any 
subsequence ys of the tail xs, and all subsequences of xs
without the head x. This way, it recursively builds all
non-empty subsequences of the input list.


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a Haskell expression that yields the infinite list 
[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,.....].

--------
Answer:

> inflist :: [Integer]
> inflist = [x | x <- [1..], _ <- [1..x]]

The outer generator picks each number x from the infinite 
list [1..]. The inner generator _ <- [1..x] is used as a
counter to repeat each x exactly x times. 

--------------------------------
Question 5.2:
Given is the inﬁnite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

Use it to deﬁne the Boolean function 
semiprime :: Integer -> Bool which returns True if and only 
if its argument is a product of exactly two prime numbers.

--------
Answer:

> semiprime :: Integer -> Bool
> semiprime n =
>   let ps = takeWhile (<= n) primes
>   in any (\x -> n `mod` x == 0 && (n `div` x) `elem` ps)
>          (takeWhile (\x -> x*x <= n) ps)

This function checks if there exists a prime x such that
n is divisible by x and the quotient n `div` x is also a
prime. Although it may seem that the list ps is computed
twice, this is not the case due to a process called 
sharing in Haskell.

--------------------------------
Question 5.3:
Given is the following deﬁnition of the inﬁnite list fs:

  fs = genfs 0 1 
    where genfs a b = a : genfs b (2*a + 3*b)

Use zip or zipWith to give an equivalent definition of the 
list fs.

--------
Answer:

Using zip: 

> fs :: [Integer]
> fs = 0 : 1 : [2*x + 3*y | (x, y) <- zip fs (tail fs)]

Alternative using zipWith:

> fs1 :: [Integer]
> fs1 = 0 : 1 : zipWith (\x y -> 2*x + 3*y) fs1 (tail fs1)

Since the function tail is not included in the functions
list (functions.md), we can simply replace it like this:

> fs2 :: [Integer]
> fs2 = 0 : 1 : zipWith (\x y-> 2*x + 3*y) fs2 (drop 1 fs2)


___________________________________________________________

6. ADT module
___________________________________________________________

The type Array a is an Abstract Data Type (ADT) for arrays 
of type a. Recall that an array is a linear data structure 
that allows indexing. In this problem, we assume that 
indexing starts from 0. The arrays in this problem are such 
that indexing an uninitialized array location yields 
Nothing, otherwise it returns Just x where x is the indexed 
value. Also, indexing out of bounds returns Nothing. 

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

To turn the below code into a module, you would create
a file named Array.hs and start it with the following
module declaration:

module Array (
    Array, create, setElement, getElement, 
    resize, size, elems  
) where

This line exports the abstract data type Array and the 
functions, but does not export the constructor AR, thus
hiding the concrete implementation details.

> data Array a = AR [Maybe a]  

> -- Creates an uninitialized array of length n
> create :: Int -> Array a
> create n
>   | n < 0     = error "Negative length"
>   | otherwise = AR (replicate n Nothing)  

> -- Sets an element at index idx to e
> setElement :: Array a -> Int -> a -> Array a
> setElement (AR xs) idx e
>   | idx < 0 || idx >= length xs 
>       = error "Index out of bounds"
>   | otherwise  
>       = AR $ take idx xs ++ [Just e] ++ drop (idx + 1) xs

> -- Gets the element at index idx
> getElement :: Array a -> Int -> Maybe a
> getElement (AR xs) idx
>   | idx < 0 || idx >= length xs = Nothing
>   | otherwise = head (drop idx xs)

> -- Resizes the array to length len
> resize :: Array a -> Int -> Array a
> resize (AR xs) len
>   | len < 0 = error "Negative length"
>   | len < length xs = AR (take len xs)
>   | otherwise = 
>       AR (xs ++ replicate (len - length xs) Nothing)

> -- Returns the size of the array
> size :: Array a -> Int
> size (AR xs) = length xs

> -- Returns the number of initialized elements
> elems :: Array a -> Int
> elems (AR xs) = length (filter isJust xs)
>   where isJust Nothing  = False
>         isJust (Just _) = True


___________________________________________________________

7. Proof on lists
___________________________________________________________

Given are the following deﬁnitions of the functions take 
and drop:

  take _ [] = []
  take n (x:xs) = if n <= 0 then [] else x:take (n-1) xs

  drop _ [] = []
  drop n (x:xs) = if n <= 0 then (x:xs) else drop (n-1) xs

Prove the following property:

  p(xs): take n xs ++ drop n xs == xs 
         for any integer n and any ﬁnite list xs

--------
Answer:

We will prove this by structural induction on the list xs.

Base case: prove p([])

    {LHS of p([])}
  take n [] ++ drop n []
=   {applying take}
  [] ++ drop n []
=   {applying drop}
  [] ++ []
=   {applying ++}
  []
    {RHS of p([])}

Inductive step: prove p((x : xs))

    Induction hypothesis:
      p(xs): take n xs ++ drop n xs == xs

  The if condition in take and drop requires us to 
  consider two cases: n <= 0 and n > 0.

  Case 1: n <= 0

    {LHS of p((x : xs))}
  take n (x : xs) ++ drop n (x : xs)
=   {applying take}
  [] ++ drop n (x : xs)
=   {applying drop}
  [] ++ (x : xs)
=   {applying ++}
  x : xs
    {RHS of p((x : xs))}

  Case 2: n > 0

    {LHS of p((x : xs))}
  take n (x : xs) ++ drop n (x : xs)
=   {applying take}
  (x : take (n - 1) xs) ++ drop n (x : xs)
=   {applying drop}
  (x : take (n - 1) xs) ++ drop (n - 1) xs
=   {applying ++}
  x : (take (n - 1) xs ++ drop (n - 1) xs)
=   {applying induction hypothesis}
  x : xs
    {RHS of p((x : xs))}

□

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

Prove for all ﬁnite expressions e: 

  isZero e ⇒ eval e == 0

--------
Answer:

We will prove this by structural induction on the 
expression e.

Base case: e = Value n

    {LHS of isZero e}
  isZero (Value n)
=   {applying isZero}
  n == 0
=   {unapplying eval}
  eval (Value n) == 0
    {RHS of isZero e}

Inductive step: e = Add a b or e = Mul a b

  Case 1: e = Add a b

    Induction hypothesis:
      isZero a ⇒ eval a == 0
      isZero b ⇒ eval b == 0

    {LHS of isZero e}
  isZero (Add a b)
=   {applying isZero}
  isZero a && isZero b
=   {applying induction hypothesis}
  eval a == 0 && eval b == 0
=   {arithmetic}
  eval a + eval b == 0
=   {unapplying eval}
  eval (Add a b) == 0
    {RHS of isZero e}

  Case 2: e = Mul a b

    Induction hypothesis:
      isZero a ⇒ eval a == 0
      isZero b ⇒ eval b == 0

    {LHS of isZero e}
  isZero (Mul a b)
=   {applying isZero}
  isZero a || isZero b
=   {applying induction hypothesis}
  eval a == 0 || eval b == 0
=   {arithmetic}
  eval a * eval b == 0
=   {unapplying eval}
  eval (Mul a b) == 0
    {RHS of isZero e}

□

___________________________________________________________