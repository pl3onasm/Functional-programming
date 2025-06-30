-----------------------------------------------------------
-- Exercise 16.1

-- defines a binary string in litte-endian style,
-- where the LSB comes first
data BString = End | Zero BString | One BString
  deriving (Eq)

instance Show BString where
  show = prettyPrint

-- displays the binary string in big-endian form,
-- where the MSB comes first
prettyPrint :: BString -> String
prettyPrint x = reverse (shw x)
  where
    shw End       = ""
    shw (Zero bs) = '0' : shw bs
    shw (One  bs) = '1' : shw bs

-- reads a bitstring and stores it into a BString;
-- the bitstring is stored in big-endian form
readBs :: String -> BString
readBs = foldl cons End
  where
    cons acc '0' = Zero acc
    cons acc '1' = One acc

-- converts an int value into a BString value
intToBs :: Int -> BString
intToBs 0 = Zero End  -- special case for 0
intToBs x = conv x 
  where
    conv 0 = End
    conv x =
      let (q, r) = x `divMod` 2
      in case r of
        0 -> Zero (conv q)
        1 -> One  (conv q)

-- converts a binary string into an int
bsToInt :: BString -> Int
bsToInt x = conv x 0 0
  where 
    conv End exp acc       = acc
    conv (Zero bs) exp acc = conv bs (exp+1) acc
    conv (One  bs) exp acc = conv bs (exp+1) (acc + 2^exp)

-- negates a binary string (bitwise negation)
bsNot :: BString -> BString
bsNot End = End
bsNot (Zero bs) = One  (bsNot bs)
bsNot (One  bs) = Zero (bsNot bs)

-- defines bitwise AND for binary strings
bsAnd :: BString -> BString -> BString
bsAnd End End = End
bsAnd End b   = bsAnd (Zero End) b  -- pad left with 0
bsAnd a   End = bsAnd a (Zero End)  -- pad right with 0
bsAnd (Zero as) (Zero bs) = Zero (bsAnd as bs)
bsAnd (Zero as) (One  bs) = Zero (bsAnd as bs)
bsAnd (One  as) (Zero bs) = Zero (bsAnd as bs)
bsAnd (One  as) (One  bs) = One  (bsAnd as bs)

-- defines bitwise OR for binary strings 
bsOr :: BString -> BString -> BString
bsOr End End = End
bsOr End b   = bsOr (Zero End) b  -- pad left with 0
bsOr a   End = bsOr a (Zero End)  -- pad right with 0
bsOr (Zero as) (Zero bs) = Zero (bsOr as bs)
bsOr (Zero as) (One  bs) = One  (bsOr as bs)
bsOr (One  as) (Zero bs) = One  (bsOr as bs)
bsOr (One  as) (One  bs) = One  (bsOr as bs)

-- increments a bit string by 1
bsInc :: BString -> BString
bsInc End       = One End
bsInc (Zero bs) = One bs
bsInc (One  bs) = Zero (bsInc bs)

  {- If we hit a Zero, we just flip it to One
     If we hit a One, we flip it to Zero, and propagate 
     the carry. If we hit the end, we append a new One.
  -}

-- computes the sum of two input bit strings, which can
-- be of different lengths
bsAdd:: BString -> BString -> BString
bsAdd x y = add x y 0
  where
    add End End 0 = End
    add End End 1 = One End
    add a   End c = add a (Zero End) c -- padding
    add End b   c = add (Zero End) b c -- padding

    add (Zero as) (Zero bs) 0 = Zero (add as bs 0)
    add (Zero as) (Zero bs) 1 = One  (add as bs 0)

    add (Zero as) (One  bs) 0 = One  (add as bs 0)
    add (Zero as) (One  bs) 1 = Zero (add as bs 1)

    add (One  as) (Zero bs) 0 = One  (add as bs 0)
    add (One  as) (Zero bs) 1 = Zero (add as bs 1)

    add (One  as) (One  bs) 0 = Zero (add as bs 1)
    add (One  as) (One  bs) 1 = One  (add as bs 1)

-----------------------------------------------------------

{- Example usage in GHCi:

  let x = readBs "10101010"
  let y = readBs "11101101110"
  let z = readBs "100111"
  
  bsAnd z y
    00000101010
  bsOr x y
    11111101110
  bsInc z
    101000
  bsAdd x y
    100000011000
  bsNot x
    01010101
  bsToInt x
    170
  bsToInt y
    1902
  bsToInt (bsAdd x y)
    2072
  intToBs 170
    10101010
-}