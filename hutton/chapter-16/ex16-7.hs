-----------------------------------------------------------
-- Exercise 16.7

{-
  We are given the Maybe type:
    
    data Maybe a = Nothing | Just a

  For this type we have the following functor instance:

    instance Functor Maybe where
      -- fmap :: (a -> b) -> Maybe a -> Maybe b
      fmap _ Nothing  = Nothing
      fmap g (Just x) = Just (g x)

  We also have the following definition for the identity
  function:

    id :: a -> a
    id x = x

  We are asked to verify the functor laws for the Maybe
  type. These laws are:

    1. fmap id      = id 
    2. fmap (g . h) = fmap g . fmap h

  We start with the first law.

    We have two cases: either we have Nothing, or a Just
    
    Case 1: Nothing
          {LHS first law}
        fmap id Nothing
      =   {applying fmap}
        Nothing
      =   {unapplying id}
        id Nothing
          {RHS first law}        

    Case 2: Just x
          {LHS first law}
        fmap id (Just x)
      =   {applying fmap}
        Just (id x)
      =   {applying id}
        Just (x)
      =   {unapplying id}
        id (Just (x))
          {RHS first law}


    We proceed with the second law.

      We have again two cases based on the structure of 
      the Maybe type.

    Case 1: Nothing
          {LHS second law}
        fmap (g . h) Nothing
      =   {applying fmap}
        Nothing

          {RHS second law}
        (fmap g . fmap h) Nothing
      =   {definition of composition}
        fmap g (fmap h Nothing)
      =   {applying fmap}
        fmap g Nothing
      =   {applying fmap}
        Nothing

      ∴ LHS = RHS
        
    Case 2: Just x
          {LHS second law}
        fmap (g . h) (Just x)
      =   {applying fmap}
        Just ((g . h) x)
      =   {definition of composition}
        Just (g (h x))

          {RHS second law}
        (fmap g . fmap h) (Just x)
      =   {definition of composition}
        fmap g (fmap h (Just x))
      =   {applying fmap}
        fmap g (Just (h x))
      =   {applying fmap}
        Just (g (h x))

      ∴ LHS = RHS

  □

-}

-----------------------------------------------------------