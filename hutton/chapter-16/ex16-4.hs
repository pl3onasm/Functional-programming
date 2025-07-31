-----------------------------------------------------------
-- Exercise 16.4

{-
  Given is the following definition:

    (++) :: [a] -> [a] -> [a]
    [] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

  The task is to verify the following two properties:

    p(xs): xs ++ [] = xs

    q(xs): xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

  We will do this by induction on lists. In particular,
  by induction on the list argument xs in p and q.

  ------------------------------------------------------
  First, we prove property p.

  Base case: p([]) holds

        {LHS of p([])}
      [] ++ []
    =   {applying ++}
      []

  Inductive case: p(x : xs) holds

      Induction hypothesis:
        xs ++ [] = xs

        {LHS of p(x : xs)}
      (x : xs) ++ []
    =   {applying ++}
      x : (xs ++ [])
    =   {induction hypothesis}
      x : xs

  ------------------------------------------------------
  Now, we prove property q.

  Base case: q([]) holds

        {LHS of q([])}
      [] ++ (ys ++ zs)
    =   {applying ++}
      ys ++ zs
    =   {unapplying ++}
      ([] ++ ys) ++ zs

  Inductive case: q(x : xs) holds

      Induction hypothesis:
        xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

        {LHS of q(x : xs)}
      (x : xs) ++ (ys ++ zs)
    =   {applying ++}
      x : (xs ++ (ys ++ zs))
    =   {induction hypothesis}
      x : ((xs ++ ys) ++ zs)
    =   {unapplying ++}
      (x : (xs ++ ys)) ++ zs
    =   {unapplying ++}
      ((x : xs) ++ ys) ++ zs

  Hence, both properties p and q hold for all lists.

  □

-}

-----------------------------------------------------------