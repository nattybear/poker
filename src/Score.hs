module Score where

import Card
import Data.Char
import Data.List
import Data.Ord
import Hand

type Score = (Maybe Category, Bool)

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

categories :: [Maybe Ranking] -> [Maybe Category]
categories = map (fmap category)

scores :: [[Card]] -> [(Maybe Category,Bool)]
scores sd = zip (categories rs) (winning rs)
  where rs = map findRanking sd

showCategory :: Category -> String
showCategory = unwords . capWords . show
  where
  capWords :: String -> [String]
  capWords = groupBy comp
    where
    comp f x = comparing isUpper f x == GT

showScore :: Score -> String
showScore (Just c,True) = showCategory c ++ " (winner)"
showScore (Just c,False) = showCategory c
showScore (Nothing,_) = ""

showDown :: String -> Score -> String
showDown s (Nothing,False) = s
showDown s sc = s ++ ' ' : showScore sc

showDowns :: [String] -> [String]
showDowns ls = zipWith showDown ls sc
  where sc = scores (map cards ls)
