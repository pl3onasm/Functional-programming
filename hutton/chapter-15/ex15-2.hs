-----------------------------------------------------------
-- Exercise 15.2

{-
  Given is the expression
    
    fst (1+2, 2+3)

  Outermost evaluation yields:

    fst (1+2, 2+3)
  = {application of fst}
    1+2
  = {evaluation of 1+2}
    3

  By contrast, innermost evaluation yields:

    fst (1+2, 2+3)
  = {evaluation of 1+2}
    fst (3, 2+3)
  = {evaluation of 2+3}
    fst (3, 5)
  = {application of fst}
    3   

  Hence, outermost evaluation requires only two steps,
  whereas innermost evaluation requires three steps to
  yield the same result. Outermost evaluation is more
  efficient and preferred, as it avoids the unnecessary
  evaluation of the second component of the pair.

-}

-----------------------------------------------------------