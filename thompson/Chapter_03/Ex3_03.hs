-----------------------------------------------------------
-- Exercise 3.3

-- | defines exclusive or, using Boolean literals
exOr :: Bool -> Bool -> Bool
exOr True True   = False
exOr True False  = True
exOr False True  = True
exOr False False = False

-----------------------------------------------------------