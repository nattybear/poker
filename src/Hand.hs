import Card

type Hand = [Card]

lg :: [a] -> (Int, [a])
lg xs = (length xs, xs)

descending :: (a -> b -> c) -> b -> a -> c
descending = flip
