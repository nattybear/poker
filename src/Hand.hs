import Card
import Data.List
import Data.Ord

type Hand = [Card]

lg :: [a] -> (Int, [a])
lg xs = (length xs, xs)

descending :: (a -> b -> c) -> b -> a -> c
descending = flip

groups :: Hand -> [[Rank]]
groups = sortBy (descending (comparing lg))
         . group . sort . map rank
