module Genetic
( finalPath
, getAllPaths
) where

import Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import Mutate
import Approxpermute


type Tour = [Int]
type Intpoint = (Int, Int)
type Pointmap = Map Int Intpoint
type Pointlist = [Intpoint]

gensize = 20
nCross = 20
nMutate = 30

getAllPaths ptList nGens seed = allPaths ptMap nGens firstGen seed
    where ptMap = makePtMap ptList
          firstGen = permutations gensize (length ptList) seed

allPaths ptMap 0     curr seed = []
allPaths ptMap nGens curr seed = next:allPaths ptMap (nGens-1) next (seed+nGens)
    where next = evolve ptMap 1 curr seed

finalPath :: Pointlist -> Int -> Int -> Maybe Pointlist
finalPath []     nGens seed = Nothing
finalPath [x]    nGens seed = Just [x]
finalPath ptList nGens seed = remap ptMap finalOrder
    where ptMap = makePtMap ptList
          firstGen   = permutations gensize (length ptList) seed
          finalOrder = head $ evolve ptMap nGens firstGen seed

remap :: Pointmap -> [Int] -> Maybe Pointlist
remap ptMap order = sequence maybeList
    where maybeList = fmap (\x -> Map.lookup x ptMap) order

evolve :: Pointmap -> Int -> [Tour] -> Int -> [Tour]
evolve ptMap 0 currentGen seed = currentGen
evolve ptMap n currentGen seed = evolve ptMap (n-1) nextGen (seed+n)
    where nextGen = selectBest ptMap $ totalGen
          totalGen = reproduce currentGen nCross nMutate seed

selectBest :: Pointmap -> [Tour] -> [Tour]
selectBest ptMap gen = take gensize sortedGen
    where sortedGen = sortBy (distCompare ptMap) gen

distCompare :: Pointmap -> Tour -> Tour -> Ordering
distCompare ptMap = on compare (getDist ptMap)
          
makePtMap :: Pointlist -> Pointmap
makePtMap ptList = Map.fromList $ zip [1..] ptList

getDist :: (Floating a) => Pointmap -> Tour -> a
getDist ptMap all@(n:ns) = getDstStrt ptMap all n

getDstStrt :: (Floating a) => Pointmap -> Tour -> Int -> a
getDstStrt ptMap [x] n = ptDist ptMap x n 
getDstStrt ptMap (x:y:ys) n = ptDist ptMap x y + getDstStrt ptMap (y:ys) n

ptDist :: (Floating a) => Pointmap -> Int -> Int -> a
ptDist ptMap x y = maybeCalc (Map.lookup x ptMap) (Map.lookup y ptMap)

maybeCalc :: (Floating a) => Maybe Intpoint -> Maybe Intpoint -> a
maybeCalc _ Nothing = error "Number does not exist in map"
maybeCalc Nothing _ = error "Number does not exist in map"
maybeCalc (Just (x1, y1))  (Just (x2, y2)) = sqrt . fromIntegral $ intInput
    where intInput = (x1 - x2)^2 + (y1 - y2)^2

