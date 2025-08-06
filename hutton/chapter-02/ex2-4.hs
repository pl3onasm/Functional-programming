-----------------------------------------------------------
-- Exercise 2.4

-- definition of last using reverse and head
last1 :: [a] -> a
last1 xs = head (reverse xs)

-- definition of last using the index operator (!!)
last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

-- definion of last using drop and head
last3 :: [a] -> a
last3 xs = head (drop (length xs - 1) xs)

-----------------------------------------------------------