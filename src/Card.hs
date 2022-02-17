import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

card :: String -> Card
card "A♥" = (Ace, Hearts)
card "K♣" = (King, Spades)

cards :: String -> [Card]
cards = map card . words
