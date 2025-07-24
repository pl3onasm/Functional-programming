import Data.Char (ord, chr)

-----------------------------------------------------------
-- Exercise 7.7

-- This module provides functions to encode and decode
-- strings into binary format and vice versa, simulating
-- a simple transmission channel.

-- We extend it with a function to add a parity bit
-- to each byte for error detection.
-- We also add a function that checks each byte for
-- parity errors during decoding.

type Bit = Int

-- Converts a binary number (LSB-first) to decimal
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- Converts a decimal number to binary (LSB-first)
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Pads a list of bits to 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Splits a list into chunks of 9 elements
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

-- Converts a string to binary (flattened)
encode :: String -> [[Bit]]
encode = map (make8 . int2bin . ord)

-- Converts binary (list of bytes) to a string
decode :: [[Bit]] -> String
decode = map (chr . bin2int)

-- Computes a parity bit (even parity)
parityBit :: [Bit] -> Bit
parityBit bits = if odd (sum bits) then 1 else 0

-- Adds parity bit to each 8-bit block creating
-- a list of 9 bits per block 
addParity :: [[Bit]] -> [[Bit]]
addParity = map (\bits -> parityBit bits : bits)

-- Checks parity, strips parity bit if correct
checkParity :: [[Bit]] -> [[Bit]]
checkParity = map check
  where
    check (p : bs)
      | p == parityBit bs = bs
      | otherwise = error "Parity error detected"
    check _ = error "Corrupted data"

-- Transmits a string through a channel
transmit :: String -> String
transmit = decode
         . checkParity
         . chop9
         . channel
         . concat
         . addParity
         . encode

-- Channel simulates transmission; may corrupt bits
channel :: [Bit] -> [Bit]
channel = id  -- No corruption here

-----------------------------------------------------------

{- Examples:

  ghci> transmit "Hello, World!"
    "Hello, World!"

  ghci> transmit "Hutton's book is quite something!"
    "Hutton's book is quite something!"

-}