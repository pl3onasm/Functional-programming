-----------------------------------------------------------
-- Exercise 3.5

-- | defines logical nAnd
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

-- | alternative definition for nAnd
nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd _    _    = True

-----------------------------------------------------------