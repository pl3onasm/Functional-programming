-----------------------------------------------------------
-- Exercise 16.3

{-
  Given are the following definitions:

    replicate :: Int -> a -> [a]
    replicate 0 _ = []
    replicate n x = x : replicate (n-1) x

    all :: (a -> Bool) -> [a] -> Bool
    all p [] = True
    all p (x : xs) = p x && all p xs

  Our task is to show that the following property p holds
  for all n ∈ ℕ:
  
    p(n): all (== x) (replicate n x)

  Intuitively, this states that replicate always returns a 
  list with n identical elements. We will prove this is 
  always true by induction on n.


  Base case: p(0) holds

        {LHS of p(0)}
      all (== x) (replicate 0 x)
    =   {applying replicate}
      all (== x) []
    =   {applying all}
      True

  
  Inductive case: p(n + 1) holds

      Induction hypothesis: 
        p(n) holds, i.e. for an arbitrary n ∈ ℕ:
          all (== x) (replicate n x)  

        {LHS of p(n + 1)}
      all (== x) (replicate (n + 1) x)
    =   {applying replicate}
      all (== x) (x : replicate n x)
    =   {applying all}
      (== x) x && all (== x) (replicate n x)
    =   {x == x = True}
      True && all (== x) (replicate n x)
    =   {induction hypothesis}
      True && True
    =   {applying conjunction}
      True

  Hence, by induction, the property holds for all n ∈ ℕ.

  □

-}

-----------------------------------------------------------