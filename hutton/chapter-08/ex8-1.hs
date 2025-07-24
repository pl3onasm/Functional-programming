-----------------------------------------------------------
-- Exercise 8.1

data Nat = Zero | Succ Nat
  deriving (Show, Eq)

-- adds two natural numbers
add :: Nat -> Nat -> Nat
add  Zero    n = n
add (Succ m) n = Succ (add m n)

-- multiplies two natural numbers, using
-- tail recursion and an accumulator
mult :: Nat -> Nat -> Nat
mult m n = mlt m n Zero
  where
    mlt  Zero    _ acc = acc
    mlt (Succ m) n acc = mlt m n (add n acc)

-- multiplies two natural numbers, using
-- non-tail recursion
mult' :: Nat -> Nat -> Nat
mult'  Zero    _ = Zero
mult' (Succ m) n = add n (mult' m n)

-----------------------------------------------------------

{- 
  The idea behind the definition for multiplication is
  that we can think of multiplication as repeated
  addition. In other words, multiplying m by n is the same
  as adding n to itself m times. It means that the first
  argument is actually used as a counter, and the second 
  argument is the value being repeatedly added to form the 
  result.

  The tail-recursive version uses an accumulator to 
  collect the result as we go along, while the
  non-tail-recursive version simply adds n to the result
  of the recursive call to mult', building up an expression
  that represents the total sum that is only evaluated
  when the recursion unwinds.

  However, Haskell's lazy evaluation means that 
  intermediate results are not computed until they are
  needed, meaning that it will not evaluate the accumulator
  during recursion and will instead build up a chain of
  deferred computations (thunks), which are only evaluated
  once the final result is demanded.
  To remedy this, we can force immediate evaluation and 
  avoid building large thunks by using the seq function, 
  which evaluates its first argument before returning its 
  second argument. 

  For example, we could modify the tail-recursive
  version of mult to use seq like this:
  mult m n = mlt m n Zero
    where
      mlt  Zero    n acc = acc
      mlt (Succ m) n acc = mlt m n (acc `seq` add n acc)

  This would ensure that the accumulator is evaluated
  at each step, preventing the buildup of thunks and
  improving performance for large inputs.


  Some examples:

  ghci> a = Succ (Succ Zero)              -- 2
  ghci> b = Succ (Succ (Succ Zero))       -- 3
  ghci> add a b
  Succ (Succ (Succ (Succ Zero)))          -- 5
  ghci> mult a b
  Succ (Succ (Succ (Succ (Succ Zero))))   -- 6
-}