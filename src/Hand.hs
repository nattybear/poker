import Card

type Hand = [Card]

descending :: (a -> b -> c) -> b -> a -> c
descending = flip
