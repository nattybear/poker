module Hand where

import Card
import Data.List
import Data.Ord

type Hand = [Card]

type Ranking = (Category, [Rank])

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

findCategory :: Hand -> Category
findCategory = categorize . lengths . groups
  where
    categorize :: [Int] -> Category
    categorize [1,1,1,1,1] = HighCard
    categorize [2,1,1,1]   = OnePair
    categorize [2,2,1]     = TwoPairs
    categorize [3,1,1]     = ThreeOfAKind
    categorize [3,2]       = FullHouse
    categorize [4,1]       = FourOfAKind

    lengths :: [[a]] -> [Int]
    lengths = map length

ranking :: Hand -> Ranking
ranking h = (findCategory h, concat (groups h)) 

category :: Ranking -> Category
category = fst

ranks :: Ranking -> [Rank]
ranks = snd
