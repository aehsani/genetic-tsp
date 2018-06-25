module Approxpermute
( permutations,
) where

import System.Random

permutations :: Int -> Int -> Int -> [[Int]]
permutations num len seed
    | num <= 0  = []
permutations num len seed = x:permutations (num-1) len seed
    where x = shuffleD list (mkStdGen (num+seed))
          list = [1..len]

shuffleD :: [a] -> StdGen -> [a]
shuffleD []   stdG = []
shuffleD [x]  stdG = [x]
shuffleD list stdG = shuffleD right stdG' ++ shuffleD left stdG''
    where (left, right) = dividePile list [] [] stdG
          (_, stdG')  = random stdG  :: (Float, StdGen)
          (_, stdG'') = random stdG' :: (Float, StdGen)

dividePile :: [a] -> [a] -> [a] -> StdGen -> ([a], [a])
dividePile []     left right stdG = (left, right)
dividePile (x:xs) left right stdG 
    | rChoice == 1 = (x:left', right')
    | rChoice == 0 = (left', x:right')
    where (left', right')  = dividePile xs left right stdG'
          (rChoice, stdG') = randomR (0,1) stdG :: (Int, StdGen)
