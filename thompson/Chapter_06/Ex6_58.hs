module Chapter_06.Ex6_58 where

import Chapter_06.Ex6_53
import Chapter_06.Ex6_56

-----------------------------------------------------------
-- Exercise 6.58

-- | Decides the winner of a trick, assuming there is a 
-- trump suit, which is passed in as the first argument
winT :: Suit -> Trick -> Player
winT trump (Trick lead cards) = snd (maximum candidates)
  where
  ldSuit   = snd (cards !! fromEnum lead)
  plrs     = [North .. West]
  -- table zipping players with their played cards
  table    = [(v, s, pl) | (pl, (v,s)) <- zip plrs cards]
  -- all trump cards (value, player)
  trpCards = [(v, pl) | (v, s, pl) <- table, s == trump]
  -- all lead suit cards (value, player)
  ldCards  = [(v, pl) | (v, s, pl) <- table, s == ldSuit]
  candidates = if null trpCards then ldCards else trpCards

-- | Example cards
c2H, c5H, cKH, cAC, cQC, c9S :: Card
c2H = (Two,   Hearts)
c5H = (Five,  Hearts)
cKH = (King,  Hearts)
cAC = (Ace,   Clubs)
cQC = (Queen, Clubs)
c9S = (Nine,  Spades)

-- | Example tricks
t1, t2, t3 :: Trick
t1 = Trick South [c2H, c5H, cKH, cAC]      
t2 = Trick West  [cAC, cQC, cKH, c5H]      
t3 = Trick North [c9S, cAC, cKH, c5H]      


-----------------------------------------------------------

{-

This extends the previous winNT function to account for a
trump suit. The trump suit is passed in as the first 
argument to the winT function. This function first 
determines the suit of the lead card, and then checks if 
any trump cards were played. If so, only those cards are
considered for winning the trick. If no trump cards were
played, only cards of the lead suit are considered. The
winner is the player who played the highest card among the
considered cards.


Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_58
ghci> winT Spades t1
South
ghci> winT Clubs t1
West
ghci> winT Diamonds t2
South
ghci> winT Clubs t2
North
ghci> winT Hearts t3
South
ghci> winT Diamonds t3
North

-}

-----------------------------------------------------------