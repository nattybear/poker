module Hand where

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

type Ranking = (Category, [Rank])

category :: Ranking -> Category
category = fst

ranks :: Ranking -> [Rank]
ranks = snd

ranking :: Hand -> Ranking
ranking h = promote (findCategory h, concat (groups h)) 
  where
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

    groups :: Hand -> [[Rank]]
    groups = sortBy (descending (comparing lg))
             . group . sort . map rank
      where
        lg :: [a] -> (Int, [a])
        lg xs = (length xs, xs)

        descending :: (a -> b -> c) -> b -> a -> c
        descending = flip

promote :: Ranking -> Ranking
promote (HighCard,[Ace,Five,_,_,_])
  = (Straight,[Five,Four,Three,Two,Ace])
promote (HighCard, rs) | isStraight rs = (Straight, rs)
promote r = r

isStraight :: [Rank] -> Bool
isStraight [h,_,_,_,l]
  = (fromEnum h) - (fromEnum l) == 4

isFlush :: Hand -> Bool
isFlush = allSame . map suit
  where allSame :: Eq a => [a] -> Bool
        allSame = (==1) . length . group

qualify :: Bool -> Ranking -> Ranking
qualify False r = r
qualify True (HighCard,rs) = (Flush,rs)
qualify True (Straight,[Ace,King,Queen,Jack,Ten])
  = (RoyalFlush,[Ace,King,Queen,Jack,Ten])
