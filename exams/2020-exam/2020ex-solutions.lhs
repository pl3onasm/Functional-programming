> import Prelude hiding (zip, flip, replicate, dropWhile,
>                        head)

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2020               
-----------------------------------------------------------

You may only use the standard arithmetic/Boolean operators 
and the standard Haskell functions included in the file 
functions.md in this folder.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression type correct? 
If YES, then give the type of the expression.

[not, []]

--------
Answer: 

It is not type correct, since lists can only contain
elements of the same type, and not is of type 
Bool -> Bool while [] is polymorphic with type [a] for 
any type a. 


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[[not],[]]

--------
Answer:

Yes, this is type correct. Here, the types of the 
elements of the list can be resolved to [Bool -> Bool].
So, the expression is a list of lists of functions
of type Bool -> Bool, i.e., the type is [[Bool -> Bool]]


--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

(&&).(&&)

--------
Answer: 

It is not type correct. The composition operator (.) 
composes two unary functions. However, (&&) is a 
binary function with type Bool -> Bool -> Bool. 
Therefore, the expression (&&) . (&&) is invalid 
because it tries to compose two binary functions, 
which (.) does not support.


--------------------------------
Question 1.4:
What is the type of the following function g?

g = not.not

--------
Answer: 

The type of g is Bool -> Bool.
This is because the composition simply chains two
functions of type Bool -> Bool, resulting in a function
that still takes a Bool and returns a Bool.


--------------------------------
Question 1.5:
What is the most general type of the following function f?

f = \x -> \y -> \z -> (x (x y), x z)

--------
Answer: 

First we note that the input x has to be a function as it
is applied to y and z. That means that y and z must be of
the same type. We also see that x is applied to its own
output, which means that the output type of x must match
the input type of x.
Thus, we can conclude that x is a function of type
a -> a, where a is some type variable, and that y and z
are of type a. So, the output of the function f is a tuple
containing two elements, both of type a.

Therefore, the most general type of f is:
  
f :: (a -> a) -> a -> a -> (a, a)


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
the order of elements in lists. Note that you are not
allowed to use the standard function sort in your
implementation.

--------
Answer:

> -- compares two lists of lists for equality ignoring 
> -- the order of elements in the lists
> cmpLOfLs :: Eq a => [[a]] -> [[a]] -> Bool 
> cmpLOfLs [] [] = True
> cmpLOfLs (xs : xss) yss =
>   case split (sameMultiset xs) yss of
>     (_, [])        -> False
>     (pre, _ : suf) -> cmpLOfLs xss (pre ++ suf)
>   where
>     sameMultiset as bs = 
>        length as == length bs &&
>        all (\x -> count x as == count x bs) as
>     count x = length . filter (== x)

> -- splits the list at the first element that satisfies p
> -- returns a tuple of the prefix and the suffix
> split :: (a -> Bool) -> [a] -> ([a], [a])
> split p xs = (takeWhile (not . p) xs, 
>               dropWhile (not . p) xs)

Since the function dropWhile is not listed in the functions
file functions.md, we can implement it as follows:

> dropWhile :: (a -> Bool) -> [a] -> [a]
> dropWhile _ [] = []
> dropWhile p (x : xs)
>   | p x       = dropWhile p xs
>   | otherwise = x : xs


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function flip such that (flip f) a b returns f b a. 
The implementation must make use of a lambda expression. 
Also, give the type of the function flip.

--------
Answer:

> flip :: (a -> b -> c) -> b -> a -> c
> flip f = \x y -> f y x


--------------------------------
Question 3.2:
Without using recursion or a list comprehension, write a 
function evenLists which takes a list of lists of Integers
as its argument and removes from it every list not 
containing an even numer. Also, give the type of this 
function.
For example:
evenLists [[1,2],[7,5,11],[1,3],[21,2,42]] 
should return: [[1,2],[21,2,42]]

--------
Answer:

> evenLists :: [[Integer]] -> [[Integer]]
> evenLists = filter (any even)


--------------------------------
Question 3.3:
Implement the funtion append such that append xs ys returns
xs++ys. You must make use of the standard function foldr, 
and are not allowed to use the ++ operator itself.

--------
Answer:

> append :: [a] -> [a] -> [a]
> append xs ys = foldr (:) ys xs

Note: recall that foldr takes a binary function, an
initial value, and a list, and essentially replaces the 
cons operator (:) with the binary function, and the empty
list with the initial value. In this case, we replace
the cons operator with itself, and the empty list with ys.
This effectively rebuilds the list xs with ys appended at 
the end, mimicking (++) without using it directly.


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

> pals :: Eq a => [[a]] -> [[a]]
> pals = filter (\xs -> xs == reverse xs)

The constraint 'Eq a' is necessary to ensure that we can
compare the elements of the lists for equality, which is
required to check if a list is a palindrome. The function
uses filter to retain only those lists that are equal to
their reverse, thus identifying palindromes.


--------------------------------
Question 3.5:
Implement the function zip using zipWith.

--------
Answer:

> zip :: [a] -> [b] -> [(a, b)]
> zip = zipWith (\x y -> (x,y))

The function zip takes two lists and pairs their
elements together into a list of tuples. It uses zipWith
to apply a lambda function that creates a tuple from
each pair of elements from the two lists. The lambda
function takes two arguments, x and y, and returns the
tuple (x, y). This effectively mimics the behavior of
the standard zip function without directly using it.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function replicate using a list 
comprehension.

--------
Answer:

> replicate :: Int -> a -> [a]
> replicate n x = [x | _ <- [1..n]]

The generator of the list comprehension is merely used
as a counter, iterating from 1 to n. The underscore 
indicates that the actual value is ignored; it just 
controls how many times x is repeated.


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

> pairs :: [a] -> [(a, a)]
> pairs xs = [(x, y) | x <- xs, y <- xs]


--------------------------------
Question 4.3:
The function pairs2 also takes a list xs and outputs a list
of pairs. A recursive implementation is given below.
For example, pairs2 [1,2,3,4] returns 
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

pairs2 [] = []
pairs2 (x : xs) = p x xs ++ pairs2 xs
  where 
    p x [] = []
    p x (y : ys) = (x,y) : p x ys

Give an equivalent implementation that makes use of (a) 
list comprehension(s) that replaces the recursions.

--------
Answer:

> pairs2 :: Eq a => [a] -> [(a, a)]
> pairs2 xs = [(x, y) | (x, n) <- zip xs [1..], 
>                        y <- drop n xs]


--------------------------------
Question 4.4:
The function perms takes a list of Ints and returns a list 
of all possible permutations of this list. 
For example, perms [1..3] should return (in this order): 

[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Implement perms using a list comprehension.

--------
Answer:

The question is not very precise about whether the input
list may contain duplicate elements or not.

If we assume that the input list only contains distinct 
elements, we can define the function as follows:

> perms :: [Int] -> [[Int]]
> perms [] = [[]]
> perms xs = [x : ys | x <- xs, 
>                      ys <- perms (filter (/= x) xs)]

However, this definition will not work for lists containing
duplicate elements, as filter (/= x) will remove all
occurrences of x, leading to incorrect permutations.

A definition that handles duplicates correctly is:

> perms' :: Eq a => [a] -> [[a]]
> perms' [] = [[]]
> perms' xs = [x : ys | x <- xs, 
>                       ys <- perms' (deleteFirst x xs)]
>   where
>     deleteFirst _ [] = []
>     deleteFirst x (y : ys)
>       | x == y    = ys
>       | otherwise = y : deleteFirst x ys

By using deleteFirst, we ensure that only the first
occurrence of x is removed, allowing for correct
permutations even when the input list contains duplicates.


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

We could define the function isPrime using the
standard function dropWhile, which is not included
in the list of available functions, but which we have
already defined in part 2:

> isPrime :: Integer -> Bool
> isPrime n = head (dropWhile (< n) primes) == n 

Here, we should also define head as it is also not 
included in the list of freely available functions:

> head :: [a] -> a
> head [] = error "head: empty list"
> head (x : _) = x

But we can also modify the definition to use takeWhile 
and elem instead, which are included in functions.md:

> isPrime' :: Integer -> Bool
> isPrime' n = n `elem` takeWhile (<= n) primes


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

> delayedFib :: [Integer]
> delayedFib = 0 : 1 : 2 : zipWith (+) 
>              (delayedFib) (drop 2 delayedFib)

This definition uses zipWith to combine the
delayedFib list with itself, offset by two elements.


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

> abc :: [String]
> abc = "a" : "b" : "c" : [x : y | y <- abc, x <- "abc"]

The list starts with the base cases: the one-letter strings
"a", "b", and "c". The rest of the list is generated by 
prepending each of 'a', 'b', and 'c' to every string 
already in abc.


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

To turn the below code into a module, you would create
a file named RLElist.hs and put at the top:

module RLElist (
    RLElist, fromList, toList, hd,
    tl, cons, cat, len, rev
) where

This line exports the RLElist type and the functions, but
does not export the constructor RLE, thus hiding the
implementation details. 

Then, you would implement the data type and functions as 
follows:

> data RLElist a = RLE [(a, Integer)]
>      deriving (Show, Eq)

> fromList :: Eq a => [a] -> RLElist a
> fromList xs = foldr cons (RLE []) xs

> toList :: RLElist a -> [a]
> toList (RLE xs) = [x | (x, n) <- xs, _ <- [1..n]]

> hd :: RLElist a -> a
> hd (RLE ((x, _) : _)) = x
> hd (RLE []) = error "hd: empty RLElist"

> tl :: RLElist a -> RLElist a
> tl (RLE ((x, 1) : xs)) = RLE xs
> tl (RLE ((x, n) : xs)) = RLE ((x, n - 1) : xs)
> tl (RLE []) = error "tl: empty RLElist"

> cons :: Eq a => a -> RLElist a -> RLElist a
> cons x (RLE []) = RLE [(x, 1)]
> cons x (RLE ((y, n) : xs))
>   | x == y    = RLE ((y, n + 1) : xs)
>   | otherwise = RLE ((x, 1) : (y, n) : xs)

> cat :: Eq a => RLElist a -> RLElist a -> RLElist a
> cat (RLE xs) (RLE ys) = RLE (merge xs ys)
>   where
>     merge [] ys = ys
>     merge [(x, n)] ((y, m) : ys)
>       | x == y    = (x, n + m) : ys
>       | otherwise = (x, n) : (y, m) : ys
>     merge ((x, n) : xs) ys = (x, n) : merge xs ys

> len :: RLElist a -> Integer
> len (RLE xs) = sum (map (\(_, n) -> n) xs)

> rev :: RLElist a -> RLElist a
> rev (RLE xs) = RLE (reverse xs)


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell function:

  f [] ys = []
  f (x:xs) ys = ys ++ f xs ys

Prove that 

  p(xs) : length xs * length ys = length(f xs ys) 
  
for all ﬁnite lists xs and ys.

--------
Answer:

We will prove this by structural induction on the list xs.

------------------------------------
1. Base case: prove p([])
------------------------------------

    {LHS of p([])}
  length [] * length ys
=   {applying definition of length}
  0 * length ys 
=   {absorbing element for multiplication}
  0
=   {unapplying the definition of length}
  length []
=   {unapplying the definition of f}
  length (f [] ys)
    {RHS of p([])}

------------------------------------
2. Inductive step: prove p((x : xs))
------------------------------------

    Induction hypothesis:
      p(xs) : length xs * length ys = length(f xs ys)
      
    {LHS of p((x : xs))}
  length (x : xs) * length ys
=   {applying definition of length}
  (1 + length xs) * length ys
=   {applying distributivity of * over +}
  1 * length ys + length xs * length ys
=   {neutral element for *}
  length ys + length xs * length ys
=   {induction hypothesis}
  length ys + length(f xs ys)
=   {we want to get this into the form where
     we can unapply the recursive case for f to get
     to the RHS and conclude the proof, so we create a
     lemma which will allow us to have:
       length ys + length(f xs ys) = length (ys ++ f xs ys)
     Generalizing the lemma we need to prove:
       length xs + length ys = length (xs ++ ys)
     See the proof below}
  length (ys ++ f xs ys)
=   {unapplying the definition for f}
  length (f (x : xs) ys)
    {RHS of p((x : xs))}

□

-------------------------------------------------------
3. Lemma: 
      q(xs) : length xs + length ys = length (xs ++ ys)
-------------------------------------------------------
   
  We will prove this lemma by structural induction on the 
  list xs

-------------------------------------
3.1 Base case: prove q([])
-------------------------------------
  
    {LHS of q([])}
  length [] + length ys
=   {applying the definition of length}
  0 + length ys
=   {neutral element for +}
  length ys
=   {unapplying the definition of ++}
  length ([] ++ ys)
    {RHS of q([])}

-------------------------------------
3.2 Inductive step: prove q((x : xs))
-------------------------------------

    Induction hypothesis:
      q(xs): length xs + length ys = length (xs ++ ys)

    {LHS of q((x : xs))}
  length (x : xs) + length ys
=   {applying the defintion of length}
  1 + length xs + length ys
=   {applying induction hypothesis}
  1 + length (xs ++ ys)
    {unapplying the definition of length}
= length (x : (xs ++ ys))
    {unapplying the definition of ++}
= length ((x : xs) ++ ys)
    {RHS of q((x : xs))}

□

___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and size:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ (x : inorder r)

  size :: Tree a -> Integer
  size Empty = 0
  size (Node x l r) = size l + 1 + size r

Prove for all finite trees t: 

  p(t): size(t) = length (inorder t)

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately.]

--------
Answer:

We prove the property p(t) by structural induction on t.

--------------------------------------
1. Base case: prove p(Empty)
--------------------------------------
 
    {LHS of p(Empty)}
  size(Empty)
=   {applying size}
  0
=   {unapplying length}
  length []
=   {unapplying inorder}
  length (inorder Empty)
    {RHS of p(Empty)}

--------------------------------------
2. Inductive step: prove p(Node x l r)
--------------------------------------

    Induction hypothesis:
      Assume p(l) and p(r) hold for subtrees l, r:
        p(l): size(l) = length (inorder l)
        p(r): size(r) = length (inorder r)

    {LHS of p(Node x l r)}
= size (Node x l r)
    {applying size}
= size l + 1 + size r
    {applying induction hypothesis}
= length(inorder l) + 1 + length(inorder r)
    {definition of length}
= length(inorder l) + length(x : inorder r)
    {applying lemma from part 7}
= length(inorder l ++ (x : inorder r))
    {unapplying inorder}
= length(inorder (Node x l r))
    {RHS of p(Node x l r)}

□

___________________________________________________________