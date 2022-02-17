import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

card :: String -> Card
card = undefined
