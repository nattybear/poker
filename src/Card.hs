import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

card :: String -> Card
card ['J', '♥'] = (Jack, Hearts)
card ['Q', '♥'] = (Queen, Hearts)
card ['K', '♥'] = (King, Hearts)
card ['A', '♥'] = (Ace, Hearts)
card ['A', '♠'] = (Ace, Spades)
card ['A', '♦'] = (Ace, Diamonds)
card ['A', '♣'] = (Ace, Clubs)
card ['K', '♣'] = (King, Clubs)

cards :: String -> [Card]
cards = map card . words

charToSuit :: Char -> Suit
charToSuit '♥' = Hearts
charToSuit '♠' = Spades
charToSuit '♣' = Clubs
charToSuit '♦' = Diamonds
