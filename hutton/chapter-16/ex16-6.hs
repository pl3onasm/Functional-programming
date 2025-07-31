-----------------------------------------------------------
-- Exercise 16.6

{-
  Given is the data type:

    data Tree = Leaf Int | Node Tree Tree

  We are to show that the number of leaves in such a tree 
  is always one greater than the number of nodes.

  In order to prove this, we first define two functions
  that count the number of leaves and nodes in a tree,
  respectively.

    cntLeaves :: Tree -> Int
    cntLeaves (Leaf _)   = 1
    cntLeaves (Node l r) = cntLeaves l + cntLeaves r

    cntNodes :: Tree -> Int
    cntNodes (Leaf _)   = 0
    cntNodes (Node l r) = 1 + cntNodes l + cntNodes r

  What we need to prove then is that the following 
  property holds for all t ∈ Tree:

    p(t): cntLeaves(t) = 1 + cntNodes(t)

  We will prove this by structural induction on t.

  Base case: p(Leaf x) holds

        {LHS of p(Leaf x)}
      cntLeaves (Leaf x)
    =   {applying cntLeaves}
      1
    =   {identity for +}
      1 + 0
    =   {unapplying cntNodes}
      1 + cntNodes (Leaf x)

  Inductive case: p(Node l r) holds

      Induction hypothesis:
        p(l): cntLeaves(l) = 1 + cntNodes(l)
        p(r): cntLeaves(r) = 1 + cntNodes(r)

        {LHS of p(Node l r)}
      cntLeaves (Node l r)
    =   {applying cntLeaves}
      cntLeaves l + cntLeaves r
    =   {induction hypothesis}
      (1 + cntNodes l) + (1 + cntNodes r)
    =   {associativity of +}
      1 + (1 + cntNodes l + cntNodes r)
    =   {unapplying cntNodes}
      1 + cntNodes (Node l r)

  Hence, the property holds for all trees of the given 
  type.

  □
  
-}