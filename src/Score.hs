module Score where

import Data.List

choose :: Int -> [a] -> [[a]]
choose n = filter ((==n) . length) . subsequences
