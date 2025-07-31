-----------------------------------------------------------
-- Exercise 16.5

{-
  Given are the following definitions:

    (++) :: [a] -> [a] -> [a]
    [] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

    take :: Int -> [a] -> [a]
    take 0 _ = []
    take _ [] = []
    take n (x:xs) = x : take (n-1) xs

    drop :: Int -> [a] -> [a]
    drop 0 xs = xs
    drop _ [] = []
    drop n (_:xs) = drop (n-1) xs

  We are to prove the following property:

    p(n, xs): take n xs ++ drop n xs = xs

  This will be proven by simultaneous induction on n ∈ ℕ
  and the list xs.

  Base case: p(0, xs) holds

        {LHS of p(0, xs)}
      take 0 xs ++ drop 0 xs
    =   {applying take and drop}
      [] ++ xs
    =   {applying ++}
      xs

  Base case: p(n, []) holds

        {LHS of p(n, [])}
      take n [] ++ drop n []
    =   {applying take and drop}
      [] ++ []
    =   {applying ++}
      []

  Inductive case: p(n + 1, x : xs) holds

      Induction hypothesis:
        take n xs ++ drop n xs = xs

        {LHS of p(n + 1, x : xs)}
      take (n + 1) (x : xs) ++ drop (n + 1) (x : xs)
    =   {applying take and drop}
      (x : take n xs) ++ (drop n xs)
    =   {applying ++}
      x : (take n xs ++ drop n xs)
    =   {induction hypothesis}
      x : xs

  Therefore, the property holds for all n ∈ ℕ and all xs.

  □

-}

-----------------------------------------------------------