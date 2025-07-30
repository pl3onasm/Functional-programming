-----------------------------------------------------------
-- Exercise 15.1

-----------------------------------------------------------
-- Exercise 15.1

{-
 a) 1 + (2 * 3)
    This expression has only one redex: (2 * 3), which can 
    be reduced to 6. It is innermost because it contains 
    no smaller redex. It is also outermost, because the 
    entire expression 1 + (2 * 3) cannot be reduced until 
    (2 * 3) is. That is, addition requires both arguments 
    to be evaluated first. So the outer expression is 
    not a redex (yet).

 b) (1 + 2) * (2 + 3)
    This expression has two redexes: (1 + 2) and (2 + 3).
    Both are innermost, in the sense they contain no 
    smaller redexes. However, under a typical left-to-right 
    evaluation strategy, (1 + 2) will be reduced first,
    and is therefore considered the innermost redex.
    The entire expression is not a redex, as multiplication 
    requires both arguments to be evaluated before it can 
    proceed, just like addition.

 c) fst (1 + 2, 2 + 3)
    This expression has three redexes: (1 + 2), (2 + 3), 
    and the entire application fst (1 + 2, 2 + 3). Both 
    (1 + 2) and (2 + 3) are innermost in the sense that 
    they contain no smaller redexes. However, reduction
    will proceed from left to right, so (1 + 2) is 
    considered the innermost redex.
    The redex fst (1 + 2, 2 + 3) is outermost: it can be 
    applied without first evaluating its arguments, 
    yielding the first element of the pair: (1 + 2).

 d) (\x -> 1 + x) (2 * 3)
    This expression has two redexes: (2 * 3), which is 
    innermost, and the entire application 
    (\x -> 1 + x) (2 * 3), which is outermost. The lambda 
    expression can be applied immediately, yielding 
    1 + (2 * 3), without first evaluating (2 * 3).
-}

-----------------------------------------------------------

