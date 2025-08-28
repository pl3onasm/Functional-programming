-----------------------------------------------------------
-- Exercise 3.1

-- | defines exclusive or, literally
-- following the description in the exercise 
exclOr :: Bool -> Bool -> Bool
exclOr x y = (x && not y) || (y && not x)

-----------------------------------------------------------