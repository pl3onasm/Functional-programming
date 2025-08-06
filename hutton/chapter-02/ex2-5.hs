-----------------------------------------------------------
-- Exercise 2.5

-- definition of init using reverse and tail
init1 :: [a] -> [a]
init1 xs = reverse (tail (reverse xs))

-- definition of init using reverse and drop
init2 :: [a] -> [a]
init2 xs = reverse (drop 1 (reverse xs))

-- definition of init using take and length
init3 :: [a] -> [a]
init3 xs = take (length xs - 1) xs

-----------------------------------------------------------