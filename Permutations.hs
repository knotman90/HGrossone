module Permutations where


permutations :: [a] -> Integer -> [[a]]
permutations [] _= [[]]
permutations _ 0 = [[]]
permutations a 1 = splitList a
permutations p@(a:xs) k = map ([a]++) (permutations p (k-1))

permTot [] _ = [[]]
permTot p@(a:xs) k = permutations p k ++ permTot (xs) k


splitList [] = [[]]
splitList (a:[]) = [[a]]
splitList (a:xs) = [[a]] ++ splitList xs

