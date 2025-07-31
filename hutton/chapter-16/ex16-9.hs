-----------------------------------------------------------
-- Exercise 16.9

{-
  We are given the Maybe type:
    
    data Maybe a = Nothing | Just a

  As we have seen in exercise 16.7, we have the 
  following functor instance for this type:
  
    instance Functor Maybe where
      -- fmap :: (a -> b) -> Maybe a -> Maybe b
      fmap _ Nothing  = Nothing
      fmap g (Just x) = Just (g x)

  The maybe type also has an applicative instance:
  
    instance Applicative Maybe where

      -- pure :: a -> Maybe a
      pure = Just

      -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
      Nothing <*> _ = Nothing
      (Just g) <*> mx = fmap g mx

  We are asked to show that this applicative instance
  satisfies the applicative laws, which are:

    1. pure id <*> x = x
    2. pure (g x) = pure g <*> pure x
    3. x <*> pure y = pure (\g -> g y) <*> x
    4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

  We will verify each law in turn. 

  -----------------------------------------------------
  Law 1: pure id <*> x = x

    Note that x is of type Maybe a, so we will have
    two cases based on the structure of x.

    Case 1: x = Nothing
      
          {LHS of law 1}
      = pure id <*> Nothing
          {applying pure}
      = Just id <*> Nothing
          {applying <*>}
      = fmap id Nothing
          {applying fmap}
      = Nothing
          {RHS of law 1}

    Case 2: x = Just a

          {LHS of law 1}
      = pure id <*> Just a 
          {applying pure}
      = Just id <*> Just a
          {applying <*>}
      = fmap id (Just a)
          {applying fmap}
      = Just (id a)
          {applying id}
      = Just a
          {RHS of law 1}

  -----------------------------------------------------
  Law 2: pure (g x) = pure g <*> pure x

    Note that x is of any type a, and g is a function
    of type a -> b. So we do not need a case analysis
    here.

      {LHS of law 2}
    = pure (g x)
        {applying pure}
    = Just (g x)

        {RHS of law 2}
    = pure g <*> pure x
        {applying pure}
    = Just g <*> Just x
        {applying <*>}
    = fmap g (Just x)
        {applying fmap}
    = Just (g x)

    ∴ LHS = RHS

  -----------------------------------------------------
  Law 3: x <*> pure y = pure (\g -> g y) <*> x

    Note that y is of any type b, but that x is of 
    type Maybe (a -> b). So we will have two cases
    based on the structure of x.  

    Case 1: x = Nothing

          {LHS of law 3}
      = Nothing <*> pure y
          {applying pure}
      = Nothing <*> Just y
          {applying <*>}
      = Nothing
         
          {RHS of law 3}
      = pure (\g -> g y) <*> Nothing
          {applying pure}
      = Just (\g -> g y) <*> Nothing
          {applying <*>}
      = fmap (\g -> g y) Nothing
          {applying fmap}
      = Nothing

      ∴ LHS = RHS

    Case 2: x = Just h

          {LHS of law 3}
      = Just h <*> pure y
          {applying pure}
      = Just h <*> Just y
          {applying <*>}
      = fmap h (Just y)
          {applying fmap}
      = Just (h y)

          {RHS of law 3}
      = pure (\g -> g y) <*> Just h
          {applying pure}
      = Just (\g -> g y) <*> Just h
          {applying <*>}
      = fmap (\g -> g y) (Just h)
          {applying fmap}
      = Just ((\g -> g y) h)
          {applying function application}
      = Just (h y)

      ∴ LHS = RHS

  -----------------------------------------------------
  Law 4: x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

    Note that x is of type Maybe (a -> b), y is of 
    type Maybe (b -> c), and z is of type Maybe c.
    So we will have two cases based on the structure
    of x.

    Case 1: x = Nothing

          {LHS of law 4}
      = Nothing <*> (y <*> z)
          {applying <*>}
      = Nothing
          
          {RHS of law 4}
      = (pure (.) <*> Nothing <*> y) <*> z
          {applying pure}
      = (Just (.) <*> Nothing <*> y) <*> z
          {applying <*>}
      = ((fmap (.) Nothing) <*> y) <*> z
          {applying fmap}
      = (Nothing <*> y) <*> z
          {applying <*>}
      = Nothing <*> z
          {applying <*>}
      = Nothing
          {LHS of law 4}

      ∴ LHS = RHS

    Case 2: x = Just h

          {LHS of law 4}
      = Just h <*> (y <*> z)
          {applying <*>}
      = fmap h (y <*> z)
          
          {RHS of law 4}
      = (pure (.) <*> Just h <*> y) <*> z
          {applying pure}
      = (Just (.) <*> Just h <*> y) <*> z
          {applying <*>}
      = (fmap (.) (Just h) <*> y) <*> z
          {applying fmap}
      = (Just ((.) h) <*> y) <*> z
          {applying <*>}
      = fmap ((.) h) y <*> z
          {applying lemma below}
      = fmap h (y <*> z)
          {LHS of law 4 }
         
      ∴ LHS = RHS


      Lemma: fmap h (y <*> z) = fmap ((.) h) y <*> z

        We can prove this lemma by considering
        the two cases based on the structure of y.

        Case 1: y = Nothing

              {LHS of lemma}
          = fmap h (Nothing <*> z)
              {applying <*>}
          = fmap h Nothing
              {applying fmap}
          = Nothing

              {RHS of lemma}
          = fmap ((.) h) Nothing <*> z
              {applying fmap}
          = Nothing <*> z
              {applying <*>}
          = Nothing 
              {LHS of lemma}

          ∴ LHS = RHS

        Case 2: y = Just g

              {LHS of lemma}
          = fmap h (Just g <*> z)
              {applying <*>}
          = fmap h (fmap g z)
              {applying functor composition law 
              for Maybe, see exercise 16.7}
          = fmap (h . g) z
              
              {RHS of lemma}
          = fmap ((.) h) (Just g) <*> z
              {applying fmap}
          = Just ((.) h g) <*> z
              {applying <*>}
          = fmap ((.) h g) z
              {function composition: h . g = (.) h g}
          = fmap (h . g) z
              {LHS of lemma}

          ∴ LHS = RHS

        □

  -----------------------------------------------------

  Thus, the Maybe type with the given applicative
  instance satisfies the applicative laws.

  □

-}

-----------------------------------------------------------