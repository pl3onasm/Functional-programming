-----------------------------------------------------------
-- Exercise 16.2

{-
  Given is the data type

    data Nat = Zero | Succ Nat

  and the definition for addition:

    add :: Nat -> Nat -> Nat
    add Zero m = m
    add (Succ n) m = Succ (add n m)

  We are also given the following properties:

    add n (Succ m) = Succ (add n m)     (1)

    add n Zero = n                      (2)

  
  We need to prove the following property p holds
  for all n ∈ Nat:

    p(n): add n m = add m n

  This will be done by induction on n.


  Base case: p(Zero) holds

        {LHS of p(Zero)}
      add Zero m
    =   {applying add}
      m
    =   {using property (2) in reverse}
      add m Zero


  Inductive case: p(Succ n) holds

      Induction hypothesis:
        p(n): add n m = add m n

        {LHS of p(Succ n)}
      add (Succ n) m
    =   {applying add}
      Succ (add n m)
    =   {induction hypothesis}
      Succ (add m n)
    =   {using property (1) in reverse}
      add m (Succ n)

  Hence, the property p holds for all n ∈ Nat.

  □

-}

-----------------------------------------------------------