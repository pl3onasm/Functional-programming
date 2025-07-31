-----------------------------------------------------------
-- Exercise 16.1

{-
  Given is the data type

    data Nat = Zero | Succ Nat

  and the definition for addition:

    add :: Nat -> Nat -> Nat
    add Zero m = m
    add (Succ n) m = Succ (add n m)

  
  We need to prove the following property p holds
  for all n ∈ Nat:

    p(n): add n (Succ m) = Succ (add n m)

  We will do this by induction on n.


  Base case: p(Zero) holds

        {LHS of p(Zero)}
      add Zero (Succ m)
    =   {applying add}
      Succ m
    =   (unapplying add)  
      Succ (add Zero m)


  Inductive case: p(Succ n) holds

      Induction hypothesis:
        add n (Succ m) = Succ (add n m)

        {LHS of p(Succ n)}
      add (Succ n) (Succ m)
    =   {applying add}
      Succ (add n (Succ m))
    =   {induction hypothesis}
      Succ (Succ (add n m))
    =   {unapplying add}
      Succ (add (Succ n) m)

  Hence, the property p holds for all n ∈ Nat.
  
  □

-}

-----------------------------------------------------------