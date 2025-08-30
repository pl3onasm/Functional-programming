import Prelude hiding ((||),(&&))

-----------------------------------------------------------
-- Exercise 3.4

-- | defines logical or
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

-- | defines logical and
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-----------------------------------------------------------