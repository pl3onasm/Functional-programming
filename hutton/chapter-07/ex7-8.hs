import Data.Char (ord, chr)

-----------------------------------------------------------
-- Exercise 7.8

-- In this exercise, we test the transmission of strings
-- through a simulated channel that can corrupt data.
-- We create some channels that simulate different
-- types of corruption, such as bit flips and lost bits.
-- We then use these channels to transmit strings and 
-- observe the results.

type Bit = Int

-- Converts binary (LSB-first) to integer
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

-- Converts integer to binary (LSB-first)
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Pads to 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Splits list into 9-bit blocks
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

-- Encoding
encode :: String -> [[Bit]]
encode = map (make8 . int2bin . ord)

-- Decoding
decode :: [[Bit]] -> String
decode = map (chr . bin2int)

-- Adds even parity bit at the front
parityBit :: [Bit] -> Bit
parityBit bits = if odd (sum bits) then 1 else 0

addParity :: [[Bit]] -> [[Bit]]
addParity = map (\bits -> parityBit bits : bits)

-- Verifies and strips parity bit
checkParity :: [[Bit]] -> [[Bit]]
checkParity = map check
  where
    check (p : bs)
      | p == parityBit bs = bs
      | otherwise = error "Parity error detected"
    check _ = error "Corrupted data"

-- Simulates transmission
transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel =
  decode
    . checkParity
    . chop9
    . channel
    . concat
    . addParity
    . encode

-- Drops the first bit
channel1 :: [Bit] -> [Bit]
channel1 [] = []
channel1 (_ : bs) = bs

-- Flips even bits (0 becomes 1)
channel2 :: [Bit] -> [Bit]
channel2 = map (\b -> if even b then 1 - b else b)

-- No corruption
channel3 :: [Bit] -> [Bit]
channel3 = id

-----------------------------------------------------------

{- Examples:

  ghci> transmit channel1 "Hello, World!"
     "*** Exception: Parity error detected
     CallStack (from HasCallStack):
     error, called at ex7-8.hs:54:21 in main:Main

  ghci> transmit channel2 "Hello, World!"
     "*** Exception: Parity error detected
     CallStack (from HasCallStack):
     error, called at ex7-8.hs:54:21 in main:Main
  
  ghci> transmit channel3 "Hello, World!"
     "Hello, World!"

-}