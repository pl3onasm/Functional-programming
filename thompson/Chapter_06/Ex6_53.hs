module Chapter_06.Ex6_53 where

-----------------------------------------------------------
-- Exercise 6.53

-- | The four suits in a standard deck of cards
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show, Enum)

-- | The value of a card 
data Value = Two | Three | Four | Five | Six | Seven
           | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Enum)

-- | A single card is a value togheter with a suit
type Card = (Value, Suit)

-- | A deck is a list of cards
type Deck = [Card]

-- | The full 52-card deck
fullDeck :: Deck
fullDeck = [(v, s) | v <- [Two .. Ace], 
                     s <- [Clubs .. Spades]]

-----------------------------------------------------------

{-

Deriving Eq, Ord, Show allows for easy comparison and 
printing of the Suit and Value types. The ordering is based
on the order of declaration, so Two < Three < ... < Ace 
for values, and Clubs < Diamonds < Hearts < Spades for 
suits.

Deriving Enum allows us to enumerate the values and suits,
enabling us to create ranges like [Two .. Ace] and
[Clubs .. Spades].

The fullDeck function uses a list comprehension to create
all combinations of values and suits, resulting in a
complete deck of 52 unique cards.

Testing in GHCi

ghci> :l Ex6_53
ghci> fullDeck
[(Two,Clubs),(Two,Diamonds),(Two,Hearts),(Two,Spades),
(Three,Clubs),(Three,Diamonds),(Three,Hearts),
(Three,Spades),(Four,Clubs),(Four,Diamonds),(Four,Hearts),
(Four,Spades),(Five,Clubs),(Five,Diamonds),(Five,Hearts),
(Five,Spades),(Six,Clubs),(Six,Diamonds),(Six,Hearts),
(Six,Spades),(Seven,Clubs),(Seven,Diamonds),(Seven,Hearts),
(Seven,Spades),(Eight,Clubs),(Eight,Diamonds),
(Eight,Hearts),(Eight,Spades),(Nine,Clubs),(Nine,Diamonds),
(Nine,Hearts),(Nine,Spades),(Ten,Clubs),(Ten,Diamonds),
(Ten,Hearts),(Ten,Spades),(Jack,Clubs),(Jack,Diamonds),
(Jack,Hearts),(Jack,Spades),(Queen,Clubs),(Queen,Diamonds),
(Queen,Hearts),(Queen,Spades),(King,Clubs),(King,Diamonds),
(King,Hearts),(King,Spades),(Ace,Clubs),(Ace,Diamonds),
(Ace,Hearts),(Ace,Spades)]

-}