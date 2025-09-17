import Test.QuickCheck

-----------------------------------------------------------
-- Exercise 7.22

-- | Zips a pair of lists into a list of pairs.
zip' :: ([a],[b]) -> [(a,b)]
zip' (xs,ys) = zip xs ys

-- | Property: zip' . unzip = id
propZipUnzip :: (Eq a, Eq b) => [(a,b)] -> Bool
propZipUnzip ts = (zip' . unzip) ts == ts

-- | Property: unzip . zip' = id
propUnzipZip :: (Eq a, Eq b) => ([a],[b]) -> Bool
propUnzipZip t = (unzip . zip') t == t

-- | Property: unzip . zip' = truncated id
propUnzipZip' :: (Eq a, Eq b) => ([a],[b]) -> Bool
propUnzipZip' (xs,ys) = 
  (unzip . zip') (xs,ys) == (take n xs, take n ys)
    where n = min (length xs) (length ys)


-----------------------------------------------------------

{-

Testing in GHCi
ghci> :load Ex7_22
ghci> quickCheck propZipUnzip
+++ OK, passed 100 tests.
ghci> quickCheck propUnzipZip
*** Failed! Falsified (after 3 tests and 1 shrink):
([],[()])
ghci> quickCheck propUnzipZip'
+++ OK, passed 100 tests.

The first property holds: unzipping a zipped list returns 
the original list. 

The second property fails in general: zipping an unzipped 
pair of lists does not always return the original pair. 
If the two lists have different lengths, the shorter one 
determines the length of the zipped list. For example, 
the counterexample ([], [()]) produces an empty zipped 
list, which unzips to a pair of empty lists ([],[]), not
the original ([],[()]).

The last property corrects this by capturing the truncation 
behavior. It expresses that unzip . zip' returns lists 
truncated to the length of the shorter input list.

-}