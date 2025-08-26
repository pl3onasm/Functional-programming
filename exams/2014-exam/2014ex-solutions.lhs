> import Prelude hiding (iterate)

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

From the two cases it is clear that zip takes two lists as
input and outputs a list of tuples whose first component
is an element of the first input list and whose second
component is an element of the second input list.
Since tuples do not require their components to have the
same type, we can write the type of zip as follows:

  zip :: [a] -> [b] -> [(a, b)]

--------------------------------
Question 1.2:
What is the type of the standard Haskell function concat?

  concat = foldr (++) []

--------
Answer:

The function partially applies foldr to the append operator
(++). The latter just concatenates two lists of the same 
type and has the signature: (++) :: [a] -> [a] -> [a]
The initial accumulator value is the empty list, which is 
polymorphic and has the type [b] for any type b. 

The function foldr has the following signature:
  foldr :: (c -> d -> d) -> d -> [c] -> d 

Partially applying it to the above inputs, yields 
c = d = [a], and b = d = [a]. Therefore, we obtain the 
following signature for concat:

  concat :: [[a]] -> [a]


--------------------------------
Question 1.3:
What is the type of the following Haskell function uncurry?

  uncurry f = (\ (a,b) -> f a b)

--------
Answer: 

The function uncurry is defined as a lambda function that 
applies a binary input function f to the two components 
of an input tuple. If f has type a -> b -> c, then the 
input tuple must have type (a, b), and the output of the
function uncurry must be of type c.
Therefore, we can deduce the following type signature:

  uncurry :: (a -> b -> c) -> (a, b) -> c


--------------------------------
Question 1.4:
What is the type of the following Haskell function plus1?
  
  plus1 = map (+ 1)

--------
Answer: 

This function partially applies map to the section (+ 1).
Since the (+) operator has the type: Num c => c -> c -> c,
and numeric literals are polymorphic, the section (+ 1) has 
type: Num c => c -> c 

We know map is a function that maps an unary input function 
to each element of an input list and has type:
  map :: (a -> b) -> [a] -> [b]

Partially applying map to the section, yields a = b = c, 
resulting in the following type signature for plus1:

  plus1 :: Num a => [a] -> [a]

That is, plus1 takes a list of numeric elements and returns
a list with each element incremented by 1.


--------------------------------
Question 1.5:
What is the type of the following Haskell function f?

  f = sum . h . g
  g = (\x -> (head x, (head . reverse) x))
  h (x,y) = [x,y]

--------
Answer: 

The inner function g takes a list x and returns a tuple
with the first and last element of the list.
So type of g is:
  g :: [a] -> (a, a)

The middle function h takes a tuple and returns a list
with the two components of the tuple. Since it takes part 
in the composition, its input type must match the output
type of g:
  h :: (a, a) -> [a]

The outer function sum takes a list of numeric elements
and returns their sum. Since the output type of h is [a],
the input type of sum must be [a] as well, and so we have:
  sum :: Num a => [a] -> a

The result of composing these three function is a unary
function that takes g's input and returns sum's output:
  f :: Num a => [a] -> a


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

> -- checks if a DCSN number is valid
> isDCSN :: Integer -> Bool
> isDCSN n = (sumProd first8Rev - last) `mod` 11 == 0
>   where
>     (first8Rev, last) = (drop 1 ds, head ds)
>     ds = revDigits n
>     revDigits 0 = [] 
>     revDigits n = (n `mod` 10) : revDigits (n `div` 10)
>     sumProd xs = sum (zipWith (*) xs [2..9])

The function revDigits extracts the digits of the input
number in reverse order, so that the last digit has index 0,
the second last digit has index 1, and so on. The first 8
digits are then fed to the function sumProd, which computes
the weighted sum of the digits using zipWith to multiply
each digit with its weight (which is index + 2). Finally,
the last digit is subtracted from the weighted sum, and we
check if the result is a multiple of 11.
Note that if a DCSN number starts with a 0, then the 
integer representation of that number will have less than
9 digits and revDigits will produce a list shorter than 9
elements. However, this does not affect the correctness of
the implementation, since the leading 0 will not contribute
to the weighted sum and zipWith just stops at the shorter
list, ignoring the leading 0.


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

> relPrimePairs :: Integer -> [(Integer, Integer)]
> relPrimePairs n = [(i, j) | i <- [2..n-1], 
>                    j <- [i+1..n], gcd i j == 1]

The list comprehension generates all relatively prime 
pairs (i, j) up to n by using two nested generators:
• The outer generator ensures i is greater than 1
  and less than n.
• The inner generator ensures j is greater than i
  and at most n.
The condition gcd i j == 1 filters pairs to include only
those that are relatively prime (i.e. share no common
factors other than 1).


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

> deck :: [(String, String)]
> deck = [(s, v) | s <- suits, v <- cards ++ honours]

The list comprehension generates all combinations of suits
and card values by iterating over each suit s in suits
and pairing it with each card value v from the concatenated
list of cards and honours. This results in a complete deck
of 52 playing cards, each represented as a tuple of suit
and value.


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

> locations :: Eq a => a -> [a] -> [Integer]
> locations n xs = [i | (x, i) <- zip xs [0..], x == n]

The function zip pairs each element of the input list xs
with its corresponding index from the infinite list [0..].
The resulting list of tuples is then filtered to consider
only those tuples where the element x matches the target
value n. The extracted i values from these tuples form the
final list of indices where n occurs in xs.


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

> iterate :: (a -> a) -> a -> [a]
> iterate f x = x : iterate f (f x)

The function iterate starts with a seed value x, and 
produces an infinite list by repeatedly applying the
function f to the previous value. The result is a list
where the head is x, the second element is f x, the 
third element is f (f x), and so on.


--------------------------------
Question 4.2:
Define the infinite list ints, which is the list of all 
integers. It should be ordered in such a way that you can 
find any given integer after searching a finite number of 
elements in ints. In other words, this is not going to 
work: ints = [0..] ++ [-1, -2..]

--------
Answer:

> ints :: [Integer]
> ints = 0 : [x | n <- [1..], x <- [n, -n]]

The list comprehension generates the integers in pairs:
for each natural number n starting from 1, it produces
the positive integer n followed by the negative integer -n.
By alternating positive and negative numbers in this way, 
every integer appears after a finite number of elements, 
ensuring the list covers all integers efficiently.
Note that the list starts with 0 as a special case, since 
it is the only integer that is neither positive nor 
negative.


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

> composites :: [Integer]
> composites = skipPrimes primes [4..]
>   where
>     skipPrimes (p : ps) (c : cs)
>       | c < p    = c : skipPrimes (p : ps) cs
>       | c == p   = skipPrimes ps cs
>       | c > p    = skipPrimes ps (c : cs)

This solution builds the list of composites by walking 
through two infinite, ordered lists in parallel: the list 
of all primes and the list of all natural numbers starting 
from 4, the smallest composite number.

The helper function skipPrimes does the actual filtering: 

• If the current composite candidate c is less than the 
current prime p, then c is a composite and is emitted, and 
we continue with the next candidate. 
• If c equals p, then c is a prime and we skip it by 
advancing in both lists. 
• If c is greater than p, then have already passed a 
smaller prime and should advance the prime list until p 
catches up to c or surpasses it.

Because one of the lists advances on every step, every 
number from 4 upward is considered in order, ensuring that 
only composite numbers are produced, with no omissions or 
duplicates.


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

> rpn :: [RPN] -> Integer
> rpn zs = evSt zs [] 
>   where 
>   evSt [] [x] = x
>   evSt (Val x : xs) st           = evSt xs (x     : st)
>   evSt (Plus  : xs) (x : y : st) = evSt xs (y + x : st)
>   evSt (Minus : xs) (x : y : st) = evSt xs (y - x : st)
>   evSt (Times : xs) (x : y : st) = evSt xs (y * x : st)
>   evSt (Div   : xs) (x : y : st) = evSt xs (div y x : st)
>   evSt _ _ = error "Invalid input"

The function rpn evaluates an RPN expression by maintaining
a stack of intermediate results. The helper function evSt
recursively processes the list of RPN tokens:
• If the input list is empty and the stack has exactly
  one element, that element is the final result.
• If the next token is a value, it is pushed onto the stack
• If the next token is an operator, the top two values
  are popped from the stack, the operation is applied,
  and the result is pushed back onto the stack.
• If the input is malformed (e.g., not enough values for
  an operator or leftover tokens at the end), an error is 
  raised.


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

To turn the below code into a module, you would create
a file named Fifo.hs and start it with the following
module declaration:

module Fifo (
  Fifo, empty, isEmpty,
  insert, top, remove
) where

This line exports the abstract data type Fifo and its
associated functions, but does not export the constructor 
FO, thus hiding the concrete implementation details.

> -- Fifo is an abstract data type for a FIFO queue
> data Fifo a = FO [a]

> -- Show instance for Fifo for pretty printing
> instance (Show a) => Show (Fifo a) where
>   show (FO xs) = "Fifo " ++ show xs

> -- creates an empty queue
> empty :: Fifo a
> empty = FO []

> -- checks if the queue is empty
> isEmpty :: Fifo a -> Bool
> isEmpty (FO []) = True
> isEmpty (FO _) = False

> -- inserts an element into the queue
> insert :: a -> Fifo a -> Fifo a
> insert x (FO xs) = FO (xs ++ [x])

> -- retrieves the 'oldest' element from the queue
> top :: Fifo a -> a
> top (FO []) = error "top from empty queue"
> top (FO (x : _)) = x

> -- removes the 'oldest' element from the queue
> remove :: Fifo a -> Fifo a
> remove (FO []) = error "remove from empty queue"
> remove (FO (_ : xs)) = FO xs


Note: A more efficient ADT would use two lists 
(front and rear) to achieve amortized constant time
complexity for insertions and removals, but the above
implementation meets the requirements.


Example usage:

ghci> x = empty
ghci> isEmpty x
True
ghci> y = insert 5 (insert 4 (insert 1 (insert 9 x)))
ghci> y
Fifo [9,1,4,5]
ghci> isEmpty y
False
ghci> top y
9
ghci> z = remove y
ghci> z
Fifo [1,4,5]
ghci> top z
1


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

  p(t): leaves t = nodes t + 1

--------
Answer:

We prove the property p(t) by structural induction on the
finite tree t.

----------------------------------------
Base case: prove p(Leaf a)
----------------------------------------

    {LHS of p(Leaf a)}
  leaves (Leaf a)
=   {applying leaves}
  1
=   {arithmetic}
  0 + 1
=   {unapplying nodes}
  nodes (Leaf a) + 1
    {RHS of p(Leaf a)}

--------------------------------------
Induction step: prove p(l) ∧ p(r)
                      => p(Node a l r)
--------------------------------------

    Induction hypotheses:
      p(l): leaves l = nodes l + 1
      p(r): leaves r = nodes r + 1

    {LHS of p(Node a l r)}
  leaves (Node a l r)
=   {applying leaves}
  leaves l + leaves r
=   {induction hypotheses p(l) and p(r)}
  (nodes l + 1) + (nodes r + 1)
=   {arithmetic}
  (1 + nodes l + nodes r) + 1
=   {unapplying nodes}
  nodes (Node a l r) + 1
    {RHS of p(Node a l r)}

□


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

  p(xs):  rev1 xs = rev2 xs 
          for all finite lists xs.

The following properties may be used without proof:

  Associativity of (++):
    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

  Concatenation with []:
    xs ++ [] = xs

--------
Answer:

We prove the property p(xs) by structural induction on the
finite list xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {LHS of p([])}
  rev1 []
=   {applying rev1}
  []
=   {applying rev2}
  rev2 []
    {RHS of p([])}

----------------------------------------
Induction step: prove p(xs) => p((x:xs))
----------------------------------------

    Induction hypothesis:
      p(xs): rev1 xs = rev2 xs

    {LHS of p(x:xs)}
  rev1 (x:xs)
=   {applying rev1}
  (rev1 xs) ++ [x]

    {RHS of p(x:xs)}
  rev2 (x:xs)
=   {applying rev2}
  shunt (x:xs) []
=   {applying shunt}
  shunt xs [x]

  At this point, we could try to apply the induction
  hypothesis p(xs) to replace rev1 xs by rev2 xs,
  but this does not help, since rev2 xs simplifies 
  to shunt xs [], not to shunt xs [x].
  The hypothesis is therefore not strong enough to
  complete the proof, and so we will use the common
  technique of generalizing the property to be
  proved to get a stronger induction hypothesis.

  So, in order to prove p((x:xs)), we need to prove
  that LHS = RHS, that is:
    (rev1 xs) ++ [x] = shunt xs [x]

  We will generalize this to prove the following
  auxiliary property q(xs):
    q(xs):    (rev1 xs) ++ ys = shunt xs ys
              for all finite lists xs and ys.

  We prove q(xs) by structural induction on the
  finite list xs.

  ----------------------------------------
  Base case: prove q([])
  ----------------------------------------

      {LHS of q([])}
    (rev1 []) ++ ys
  =   {applying rev1}
    [] ++ ys
  =   {applying shunt}
    shunt [] ys
      {RHS of q([])}

  ----------------------------------------
  Induction step: prove q(xs) => q((x:xs))
  ----------------------------------------

      Induction hypothesis:
        q(xs): (rev1 xs) ++ ys = shunt xs ys

      {LHS of q(x:xs)}
    (rev1 (x:xs)) ++ ys
  =   {applying rev1}
    ((rev1 xs) ++ [x]) ++ ys
  =   {associativity of (++), given without proof}
    (rev1 xs) ++ ([x] ++ ys)
  =   {induction hypothesis q(xs)}
    shunt xs ([x] ++ ys)
  =   {applying shunt}
    shunt (x:xs) ys
      {RHS of q(x:xs)}    

  This completes the proof of the auxiliary 
  property q(xs).
  □

  By instantiating ys to [x] in q(xs), we obtain:
    (rev1 xs) ++ [x] = shunt xs [x]
  which completes the proof of p(x:xs).
  
□

___________________________________________________________