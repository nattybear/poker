import Card
import Data.List
import Data.Ord

type Hand = [Card]

data Category
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  deriving (Eq,Ord)

lg :: [a] -> (Int, [a])
lg xs = (length xs, xs)

descending :: (a -> b -> c) -> b -> a -> c
descending = flip

groups :: Hand -> [[Rank]]
groups = sortBy (descending (comparing lg))
         . group . sort . map rank

lengths :: [[a]] -> [Int]
lengths = map length
