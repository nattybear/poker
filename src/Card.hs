import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst
