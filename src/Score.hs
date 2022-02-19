module Score where

import Card
import Data.List
import Hand

choose :: Int -> [a] -> [[a]]
choose n = filter ((==n) . length) . subsequences

bestRanking :: [Card] -> Ranking
bestRanking = maximum . map ranking . choose 5
