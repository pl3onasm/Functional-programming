-----------------------------------------------------------
-- Exercise 12.5

{-
  1. Preservation of identity:
     pure id <*> x = x

    LHS
      id :: a -> a
      pure id :: f (a -> a)                   (def of pure)
      x :: f a                               (def of (<*>))
      pure id <*> x :: f a 
    
    RHS
      x :: f a

    So both sides yield the same type f a.

  2. Preservation of application:
     pure (g x) = pure g <*> pure x
    
    RHS
      pure g :: f (a -> b)                   (def of (<*>))
      pure x :: f a                          (def of (<*>))
      pure g <*> pure x :: f b               (def of (<*>))

    LHS
      g :: a -> b                             (def of pure)
      x :: a                                  (def of pure)
      pure (g x) :: f b                       (def of pure)

    So both sides yield the same type f b.

  3. Interaction of pure and <*>:
     x <*> pure y = pure (\g -> g y) <*> x

    LHS
      x :: f (a -> b)                        (def of (<*>))
      pure y :: f a                          (def of (<*>))
      y :: a                                 (def of pure)                  
      x <*> pure y :: f b                    (def of (<*>))

    RHS
      x :: f (a -> b)                                 (LHS)
      y :: a                                          (LHS)
      g :: a -> b                            (def of (<*>))
      pure (\g -> g y) :: f (a -> b)          (def of pure)
      pure (\g -> g y) <*> x :: f b          (def of (<*>))

    So both sides yield the same type f b.

    
  4. Associativity of <*>:
     x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

    LHS
      y :: f (a -> b)                        (def of (<*>))
      z :: f a                               (def of (<*>))
      y <*> z :: f b                         (def of (<*>))
      x :: f (b -> c)                        (def of (<*>))
      x <*> (y <*> z) :: f c                 (def of (<*>))

    RHS
      z :: f a                                        (LHS)
      y :: f (a -> b)                                 (LHS)
      x :: f (b -> c)                                 (LHS)
      (.) :: (b -> c) -> (a -> b) -> (a -> c)  (def of (.))
      pure (.) <*> x <*> y :: f (a -> c)      (def of pure)
      pure (.) <*> x <*> y <*> z :: f c      (def of (<*>))

    So both sides yield the same type f c.
-}



-----------------------------------------------------------