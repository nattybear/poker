import Rank
import Suit

type Card = (Rank, Suit)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

card :: String -> Card
card [r, s] = (charToRank r, charToSuit s)

cards :: String -> [Card]
cards = map card . words

charToSuit :: Char -> Suit
charToSuit '♥' = Hearts
charToSuit '♠' = Spades
charToSuit '♣' = Clubs
charToSuit '♦' = Diamonds

charToRank :: Char -> Rank
charToRank '2' = Two
charToRank '3' = Three
charToRank '4' = Four
charToRank '5' = Five
charToRank '6' = Six
charToRank '7' = Seven
charToRank '8' = Eight
charToRank '9' = Nine 
charToRank 'T' = Ten
charToRank 'J' = Jack
charToRank 'Q' = Queen
charToRank 'K' = King
charToRank 'A' = Ace
