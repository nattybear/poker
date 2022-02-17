import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

card :: String -> Card
card ['J', s] = (Jack, charToSuit s)
card ['Q', s] = (Queen, charToSuit s)
card ['K', s] = (King, charToSuit s)
card ['A', s] = (Ace, charToSuit s)

cards :: String -> [Card]
cards = map card . words

charToSuit :: Char -> Suit
charToSuit '♥' = Hearts
charToSuit '♠' = Spades
charToSuit '♣' = Clubs
charToSuit '♦' = Diamonds
