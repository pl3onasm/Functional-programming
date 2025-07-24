-----------------------------------------------------------
-- Exercise 12.8

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do x <- st
                 return (g x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do f <- stf
                   x <- stx
                   return (f x)

instance Monad ST where
  -- return :: a -> ST a
  return = pure
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s 
                      in app (f x) s')

-----------------------------------------------------------

{-
  In the Functor instance, do-notation (via >>=) is used to
  extract x from st. The function g is then applied to x, 
  and the result is rewrapped using return.

  In the Applicative instance, <*> is defined by 
  sequentially extracting f (a function) from stf and x 
  from stx, and then returning the result of applying f to 
  x.

  The Monad instance is the same as the standard ST monad,
  where >>= explicitly threads the state through st and f 
  x.

  Using the Monad instance makes the Functor and 
  Applicative implementations much simpler. We no longer 
  have to manually manage the state s; instead, state 
  threading is handled implicitly by the monadic 
  operations under the hood (or below the waterline, as 
  professor Hutton would say).
-}