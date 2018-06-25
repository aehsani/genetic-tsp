module Genetic
( finalPath
) where

import Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import Mutate
import Approxpermute

type Pointmap = Map Int (Int, Int)

gensize = 20
nChildren = 50

finalPath :: [(Int, Int)] -> Int -> Int -> Maybe [(Int, Int)]
finalPath []     nGens seed = Just [] -- maybe this should be nothing - pay more attention when refactoring
finalPath ptList nGens seed = remap ptMap finalOrder
    where ptMap = makePtMap ptList
          firstGen   = permutations gensize (length ptList) seed
          finalOrder = head $ evolve ptMap nGens firstGen seed

remap :: Pointmap -> [Int] -> Maybe [(Int, Int)]
remap ptMap order = sequence maybeList
    where maybeList = fmap (\x -> Map.lookup x ptMap) order

evolve :: Pointmap -> Int -> [[Int]] -> Int -> [[Int]]
evolve ptMap 0 currentGen seed = currentGen
evolve ptMap n currentGen seed = evolve ptMap (n-1) nextGen (seed+n)
    where nextGen = selectBest ptMap $ reproduce currentGen nChildren seed

selectBest :: Pointmap -> [[Int]] -> [[Int]]
selectBest ptMap gen = take gensize sortedGen
    where sortedGen = sortBy (distCompare ptMap) gen

distCompare :: Pointmap -> [Int] -> [Int] -> Ordering
distCompare ptMap = on compare (getDist ptMap)
          
makePtMap :: [(Int, Int)] -> Pointmap
makePtMap ptList = Map.fromList $ zip [1..] ptList

getDist :: (Floating a) => Pointmap -> [Int] -> a
getDist ptMap [] = error "No points to get distance between"
getDist ptMap all@(n:ns) = getDstStrt ptMap all n

getDstStrt :: (Floating a) => Pointmap -> [Int] -> Int -> a
getDstStrt ptMap [] n = error "No points to get distance between"
getDstStrt ptMap [x] n = ptDist ptMap x n 
getDstStrt ptMap (x:y:ys) n = ptDist ptMap x y + getDstStrt ptMap (y:ys) n

ptDist :: (Floating a) => Pointmap -> Int -> Int -> a
ptDist ptMap x y = maybeCalc (Map.lookup x ptMap) (Map.lookup y ptMap)

maybeCalc :: (Floating a) => Maybe (Int, Int) -> Maybe (Int, Int) -> a
maybeCalc _ Nothing = error "Number does not exist in map"
maybeCalc Nothing _ = error "Number does not exist in map"
maybeCalc (Just (x1, y1))  (Just (x2, y2)) = sqrt . fromIntegral $ intInput
    where intInput = (x1 - x2)^2 + (y1 - y2)^2

