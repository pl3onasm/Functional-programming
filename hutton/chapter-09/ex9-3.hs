-----------------------------------------------------------
-- Exercise 9.3

{-
  If split is generalized to include pairs with empty 
  lists, then solutions may no longer terminate.

  Allowing empty lists means there is no guarantee that 
  exprs will split an input list into two strictly smaller 
  sublists. As a result, the recursion may fail to reach 
  the base cases.

  For instance, repeatedly splitting off an empty list 
  could cause the same expression to be rebuilt over and 
  over again, leading to an infinite loop or stack 
  overflow.

  Restricting split to non-empty pairs ensures 
  that each recursive call works on smaller sublists, 
  eventually reaching the base case and producing only 
  valid expressions.
-}

-----------------------------------------------------------