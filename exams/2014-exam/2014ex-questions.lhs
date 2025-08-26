-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2014               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the standard Haskell function zip?

  zip (x : xs) (y : ys) = (x,y) : zip xs ys
  zip xs       ys       = []

--------
Answer: 



--------------------------------
Question 1.2:
What is the type of the standard Haskell function concat?

  concat = foldr (++) []

--------
Answer:



--------------------------------
Question 1.3:
What is the type of the following Haskell function uncurry?

  uncurry f = (\ (a,b) -> f a b)

--------
Answer: 



--------------------------------
Question 1.4:
What is the type of the following Haskell function plus1?
  
  plus1 = map (+ 1)

--------
Answer: 



--------------------------------
Question 1.5:
What is the type of the following Haskell function f?

  f = sum . h . g
  g = (\x -> (head x, (head . reverse) x))
  h (x,y) = [x,y]

--------
Answer: 




___________________________________________________________

2. Programming in Haskell
___________________________________________________________

A Dutch Citizen Service Number (DCSN) has always 9 digits 
and the first digit can be a 0. Many websites use the 
following rudimentary check to validate the correctness of 
the (9 digit) number ABCDEF GHI. 

First compute:
X = 9 x A + 8 x B + 7 x C + 6 x D + 5 x E 
    + 4 x F + 3 x G + 2 x H - 1 x I. 

Note that the last digit has a negative weight. If X is a 
multiple of 11, then the number ABCDEF GHI passes the test, 
otherwise it is invalid.

Write a Haskell functie isDCSN (including its type) that 
determines whether its argument passes the test described 
above.

For example:

  isDCSN 123456782 = True
  isDCSN 123456789 = False
  isDCSN 012345672 = True

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
Write a function relPrimePairs n that returns the list of 
pairs (i,j) where 1 < i < j <= n and i and j have no common 
factor (you may use the function gcd that computes the 
greatest common divisor of its two arguments). The
implementation of relPrimePairs must be a list 
comprehension.

Example:    relPrimePairs 7 
          = [(2,3),(2,5),(2,7),(3,4),(3,5),(3,7),
             (4,5),(4,7),(5,6),(5,7),(6,7)]

--------
Answer:

>
>
>


--------------------------------
Question 3.2:
Given are the Haskell definitions of suits, cards and 
honours:

> suits = ["Clubs", "Diamonds", "Hearts", "Spades"]
> cards = map show [2..10]
> honours = ["J","Q","K","A"]

Write a list comprehension for deck, where deck is:

  [("Clubs","2"),("Clubs","3"),("Clubs","4"),
  ("Clubs","5"),("Clubs","6"),("Clubs","7"),
  ("Clubs","8"),("Clubs","9"),("Clubs","10"),
  ("Clubs","J"),("Clubs","Q"),("Clubs","K"),
  ("Clubs","A"),("Diamonds","2"),("Diamonds","3"),
  ("Diamonds","4"),("Diamonds","5"),("Diamonds","6"),
  ("Diamonds","7"),("Diamonds","8"),("Diamonds","9"),
  ("Diamonds","10"),("Diamonds","J"),("Diamonds","Q"),
  ("Diamonds","K"),("Diamonds","A"),("Hearts","2"),
  ("Hearts","3"),("Hearts","4"),("Hearts","5"),
  ("Hearts","6"),("Hearts","7"),("Hearts","8"),
  ("Hearts","9"),("Hearts","10"),("Hearts","J"),
  ("Hearts","Q"),("Hearts","K"),("Hearts","A"),
  ("Spades","2"),("Spades","3"),("Spades","4"),
  ("Spades","5"),("Spades","6"),("Spades","7"),
  ("Spades","8"),("Spades","9"),("Spades","10"),
  ("Spades","J"),("Spades","Q"),("Spades","K"),
  ("Spades","A")]

--------
Answer:

>
>
>


--------------------------------
Question 3.3:
Use a list comprehension and the function zip to write
a Haskell function locations n xs that returns the list of
all indexes i such that the i-th element of xs is n 
(i.e. xs!!i == n). Note that the first elelement of a list 
has index 0. You are not allowed to use the indexing 
operator !!, nor are you allowed to implement it yourself.

Example:    locations 0 [x `mod` 10 | x <- [1..50]]
          = [9,19,29,39,49]

--------
Answer:

>
>
>



___________________________________________________________

4. Infinite lists
___________________________________________________________

Question 4.1:
The function iterate creates an infinite list where the 
first element is the given starting value, the second 
element is obtained by applying the given function to the 
starting value, the third element by applying the function 
to the second element, and so on. 

For example, iterate (2*) 1 yields the infinite list 
[2,4,8,16,32,64,128,256,512,...]. Give a Haskell
implementation (including its type) of the function 
iterate.

--------
Answer:

>
>
>


--------------------------------
Question 4.2:
Define the infinite list ints, which is the list of all 
integers. It should be ordered in such a way that you can 
find any given integer after searching a finite number of 
elements in ints. In other words, this is not going to 
work: ints = [0..] ++ [-1, -2..]

--------
Answer:

>
>
>


--------------------------------
Question 4.3:
Given is the definition of the infinite list of primes:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

Use primes to define the infinite list composites of non-
primes. So, take 10 composites should yield
[4,6,8,9,10,12,14,15,16,18]. Note that we skip the value 1.

--------
Answer:

>
>
>



___________________________________________________________

5. Reverse Polish Notation
___________________________________________________________

We are used to write expressions using infix notation. For 
instance, we write 10 - (4 + 3) * 2. The downside of this 
notation is that we have to use parentheses to denote 
precedence. Reverse Polish Notation (RPN) is another way of 
writing down expressions, and does not need parentheses. 
In RPN, every operator follows its operands, therefore RPN 
is also called postfix notation. The above expression in 
RPN is: 10 4 3 + 2 * -

Evaluating such an expression goes as follows. We keep 
pushing numbers onto a stack, until we encounter the first 
operator. So, when we encounter the +, the stack contains 
[3, 4, 10] (here, the head of the list is the top of the 
stack). We replace the two top numbers from the stack by 
their sum. The stack is now [7, 10]. Next, we push 2 on 
the stack (so, [2, 7, 10]). Now, we encounter an operator 
again, we pop 2 and 7 off the stack, apply the operator 
and push the result to the stack yielding [14, 10]. 
Finally, there is a -. We pop 10 and 14 from the stack, 
subtract 14 from 10 and push that back. The number on the
stack is now -4, which is the final result.

We use the following data type for representing RPN 
literals:

> data RPN = Val Integer | Plus | Minus | Times | Div

Write a Haskell funtion rpn :: [RPN] -> Integer that 
evaluates an RPN expression to an Integer.

Two examples:

  rpn [Val 10, Val 2, Div] = 5

  rpn [Val 10, Val 4, Val 3, 
       Plus, Val 2, Times, Minus] = -4

--------
Answer:

>
>
>
>
>


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) Fifo tp implements a simple 
data type for the storage of elements of the type tp, from 
which elements are retrieved in the same order as in which 
they are inserted: FIFO stands for First In First Out 
queue.

Implement a module Fifo such that the concrete 
implementation of the type Fifo is hidden from the user.

The following operations on the data type Fifo must be 
implemented:
• empty returns an empty queue.
• isEmpty returns True for an empty queue, 
  otherwise False.
• insert: returns the queue that is the result 
  of inserting an element.
• top: returns the 'oldest' element of the queue.
• remove: returns the queue that is obtained by 
  removing the 'oldest' element.

--------
Answer:

> 
>
>


___________________________________________________________

7. Proof on trees
___________________________________________________________

Given is the data type Tree:

  data Tree a = Leaf a | Node a (Tree a) (Tree a)

Given are the functions leaves and nodes:

  leaves (Leaf _) = 1
  leaves (Node a l r) = leaves l + leaves r

  nodes (Leaf _) = 0
  nodes (Node a l r) = 1 + nodes l + nodes r

Prove for all finite trees t: 

  leaves t = nodes t + 1

--------
Answer:







___________________________________________________________

8. Proof on lists
___________________________________________________________

Given are the definitions of the functions rev1, shunt, 
and rev2:

  rev1 :: [a] -> [a]
  rev1 [] = []
  rev1 (x:xs) = (rev1 xs) ++ [x]

  shunt :: [a] -> [a] -> [a]
  shunt [] ys = ys
  shunt (x:xs) ys = shunt xs (x:ys)

  rev2 :: [a] -> [a]
  rev2 xs = shunt xs []

Prove the following property p: 

  rev1 xs = rev2 xs 
  for all finite lists xs.

The following properties may be used without proof:

  Associativity of (++):
    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

  Concatenation with []:
    xs ++ [] = xs

--------
Answer:







___________________________________________________________