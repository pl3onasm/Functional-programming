-----------------------------------------------------------
-- Exercise 12.3

instance Applicative ((->) a) where
  
  -- generally:  pure :: b -> f b
  -- substituting f with ((->) a) gives us:
  -- pure :: b -> (a -> b)
  pure x = \_ -> x

  -- generally: (<*>) :: f (b -> c) -> f b -> f c
  -- substituting f with ((->) a) gives us:
  -- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
  (<*>) f g = \x -> f x (g x)

-----------------------------------------------------------

{- 
  The Applicative instance for the function type ((->) a)
  describes computations that depend on some shared input 
  of type a.

  1. pure takes a value and returns a function that ignores
     its argument and always returns that value.
     So: pure x = \_ -> x 
     or equivalently, pure x = const x

  2. (<*>) takes two functions:
     - f :: a -> (b -> c)
     - g :: a -> b
     and returns a new function h of type a -> c,
     where h x = f x (g x)

     In other words, the function f receives an input x of 
     type a and produces a function of type b -> c, whereas
     g produces a value of type b for the same input x.
     To get the final result of type c, we apply the 
     function produced by f x to the value produced by g x.
     So: f <*> g = \x -> f x (g x)

  The Applicative instance for ((->) a) satisfies the
  Applicative laws:

  1. Preservation of identity:
     pure id <*> x = x
    
    LHS:
    We have pure id = \_ -> id
    So:
    (\_ -> id) <*> x  
      = \a -> ((\_ -> id) a) (x a)
      = \a -> id (x a)
      = \a -> x a
    RHS:
    x = \a -> x a

    So both sides yield the same function \a -> x a

  2. Preservation of application:
     pure (g x) = pure g <*> pure x

    LHS:
    pure (g x) = \_ -> g x

    RHS:
    We have pure g = \_ -> g and pure x = \_ -> x
    So:
    (\_ -> g) <*> (\_ -> x)
      = \a -> ((\_ -> g) a) ((\_ -> x) a)
      = \a -> g (x)  -- since both are constant functions
      = \_ -> g x

    So both sides yield the constant function \_ -> g x

  3. Interaction of pure and <*>:
     x <*> pure y = pure (\g -> g y) <*> x

    LHS:
    x <*> (\_ -> y)
      = \a -> x a ((\_ -> y) a)
      = \a -> x a y

    RHS:
    We have pure (\g -> g y) = \_ -> \g -> g y
    So:
    (\_ -> \g -> g y) <*> x
      = \a -> ((\_ -> \g -> g y) a) (x a)
      = \a -> (\g -> g y) (x a)
      = \a -> x a y   -- since x a is a function g, 
                      -- we apply it to y

    So both sides reduce to the same function \a -> x a y

    4. Associativity of <*>:
     x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

    LHS:
    x <*> (y <*> z)
      = \a -> x a ((y <*> z) a)
      = \a -> x a (y a (z a))

    RHS:
    We have pure (.) = \_ -> (.)
    So:
    (\_ -> (.)) <*> x
      = \a -> ((\_ -> (.)) a) (x a)
      = \a -> (.) (x a)
      = \a -> \g -> x a . g   

    Now we apply this to y
    (\a -> \g -> x a . g) <*> y
      = \a -> (\g -> x a . g) (y a)
      = \a -> x a . y a

    Finally, we apply this to z
    (\a -> x a . y a) <*> z
      = \a -> (x a . y a) (z a)
      = \a -> x a (y a (z a))

    So both sides reduce to the same function:
    \a -> x a (y a (z a))
 

  Thus, the Applicative instance for ((->) a) is valid.
-}