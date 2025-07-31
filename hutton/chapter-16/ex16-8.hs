-----------------------------------------------------------
-- Exercise 16.8

{-
  We are given the following data type:

    data Tree a = Leaf a | Node (Tree a) (Tree a)

  Also given is the following functor instance:

    instance Functor Tree where
      -- fmap :: (a -> b) -> Tree a -> Tree b
      fmap g (Leaf x) = Leaf (g x)
      fmap g (Node l r) = Node (fmap g l) (fmap g r)

  Our task is to verify the functor laws for this  
  data type. This means we need to prove the following
  properties for all t ∈ Tree a:

    p(t): fmap id t      = t 
    q(t): fmap (g . h) t = fmap g (fmap h t)

  These properties are adapted for the Tree type from 
  the general functor laws. We will prove these 
  properties by structural induction on trees.

  ----------------------------------------------------
  We start with property p(t)

    Base case: p(Leaf x) holds

          {LHS of p(Leaf x)}
      = fmap id (Leaf x)
          {applying fmap}
      = Leaf (id x)
          {applying id}
      = Leaf x
          {RHS}

    Inductive case: p(Node l r) holds

      Induction hypothesis: 
        p(l): fmap id l = l
        p(r): fmap id r = r

          {LHS of p(Node l r)}
      = fmap id (Node l r)
          {applying fmap}
      = Node (fmap id l) (fmap id r)
          {induction hypothesis}
      = Node l r
          {RHS}

  ----------------------------------------------------
  We proceed with property q(t)

    Base case: q(Leaf x) holds

          {LHS of q(Leaf x)}
      = fmap (g . h) (Leaf x)
          {applying fmap}
      = Leaf ((g . h) x)
          {definition of composition}
      = Leaf (g (h x))

          {RHS of q(Leaf x)}
      = fmap g (fmap h (Leaf x))
          {applying fmap}
      = fmap g (Leaf (h x))
          {applying fmap}
      = Leaf (g (h x))

      ∴ LHS = RHS

    Inductive case: q(Node l r) holds

      Induction hypothesis:
        q(l): fmap (g . h) l = fmap g (fmap h l)
        q(r): fmap (g . h) r = fmap g (fmap h r)

          {LHS of q(Node l r)}
      = fmap (g . h) (Node l r)
          {applying fmap}
      = Node (fmap (g . h) l) (fmap (g . h) r)
          {induction hypothesis}
      = Node (fmap g (fmap h l)) (fmap g (fmap h r))
          
          {RHS of q(Node l r)}
      = fmap g (fmap h (Node l r))
          {applying fmap}
      = fmap g (Node (fmap h l) (fmap h r))
          {applying fmap}
      = Node (fmap g (fmap h l)) (fmap g (fmap h r))

      ∴ LHS = RHS

  Therefore, the functor laws hold for all trees of the
  given type.
    
  □

-}

-----------------------------------------------------------