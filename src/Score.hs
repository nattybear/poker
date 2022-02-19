module Score where

import Card
import Data.List
import Hand

choose :: Int -> [a] -> [[a]]
choose n = filter ((==n) . length) . subsequences

bestRanking :: [Card] -> Ranking
bestRanking = maximum . map ranking . choose 5

findRanking :: [Card] -> Maybe Ranking
findRanking cs
  | length cs /= 7 = Nothing
  | otherwise = Just (bestRanking cs)

winning :: Ord a => [Maybe a] -> [Bool]
winning rs = map wins rs
  where
  wins Nothing = False
  wins r       = r == maximum rs
