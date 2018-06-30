module Mutate
( reproduce
) where

import System.Random
import Approxpermute

type Ordrep = [Int]
type Canonic = [Int]
type Tour = [Int]

reproduce :: [Tour] -> Int -> Int -> Int -> [Tour]
reproduce []   nCross nMut seed = []
reproduce curr nCross nMut seed = (curr ++ children)
    where children = crossed ++ mutated
          crossed = crossGen curr nCross stdGen
          mutated = mutateGen curr nMut stdGen'
          (stdGen,stdGen') = (mkStdGen seed, mkStdGen (seed+97))

mutateGen :: [Tour] -> Int -> StdGen -> [Tour]
mutateGen curr 0    stdGen = []
mutateGen curr nMut stdGen = x:mutateGen curr (nMut-1) stdGen''
    where (x, stdGen'') = mutSwap p stdGen' 
          (p, stdGen')  = selectOne curr stdGen

selectOne :: [Tour] -> StdGen -> (Tour, StdGen)
selectOne gen stdGen = (p, stdGen')
    where p = gen !! pos
          (pos, stdGen') = randomR (0,bound) stdGen
          bound = length gen - 1

mutSwap :: Tour -> StdGen -> (Tour, StdGen)
mutSwap path stdGen = (newPath, stdGen'')
    where newPath = swapPos i j path
          (i, stdGen')  = randomR (0,bound) stdGen
          (j, stdGen'') = randomR (0,bound) stdGen'
          bound = length path - 1

swapPos :: Int -> Int -> Tour -> Tour
swapPos i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

crossGen :: [Tour] -> Int -> StdGen -> [Tour]
crossGen curr 0      stdGen = []
crossGen curr nCross stdGen = x:crossGen curr (nCross-1) stdGen''
    where (x, stdGen'')   = crossTour p q stdGen'
          (p, q, stdGen') = selectTwo curr stdGen

selectTwo :: [Tour] -> StdGen -> (Tour, Tour, StdGen)
selectTwo gen stdGen = (p, q, stdGen')
    where p = gen !! pos
          q = gen !! fst (randomR (0,bound) stdGen')
          (pos, stdGen') = randomR (0,bound) stdGen
          bound = length gen - 1

crossTour :: Tour -> Tour -> StdGen -> (Tour, StdGen)
crossTour p q stdGen = (getBackTour newOrdrep, stdGen')
    where (newOrdrep, stdGen') = switchOrd pOrd qOrd stdGen
          pOrd = getOrdrep p
          qOrd = getOrdrep q

switchOrd :: Ordrep -> Ordrep -> StdGen -> (Ordrep, StdGen)
switchOrd p  q  stdGen = (leftp ++ rightq, stdGen')
    where leftp        = take n p
          rightq       = drop n q
          (n, stdGen') = randomR (lbound, ubound) stdGen
          ubound       = min (length p) (length q)
          lbound       = if ubound == 0 then 0 else 1

tourToOrd :: Tour -> Canonic -> Ordrep -> Ordrep
tourToOrd t  []  ord = ord
tourToOrd [] can ord = ord
tourToOrd (x:xs) can ord = tourToOrd xs can' (pos:ord)
    where (pos, can')    = findSpan x can

getOrdrep :: Tour -> Ordrep
getOrdrep t = reverse $ tourToOrd t (take len [1..]) []
    where len = length t

findSpan :: Int -> [Int] -> (Int, [Int])
findSpan x [] = error "Found element not in List"
findSpan x (y:ys)
    | x == y  = (1, ys)
findSpan x (y:ys) = (1+pos', y:rest)
    where (pos', rest) = findSpan x ys

ordToTour :: Ordrep -> Canonic -> Tour -> Tour
ordToTour []  can t = t
ordToTour ord []  t = t
ordToTour (n:ns) can t = ordToTour ns can' (x:t)
    where (x, can') = getElemSpan n can

getBackTour :: Ordrep -> Tour
getBackTour ord = reverse $ ordToTour ord (take len [1..]) []
    where len = length ord

getElemSpan :: Int -> [Int] -> (Int, [Int])
getElemSpan n can
    | n > length can  = error "Not enough elements in Canonic tour"
getElemSpan 1 (x:xs) = (x, xs)
getElemSpan n (x:xs) = (y, x:rest)
    where (y, rest) = getElemSpan (n-1) xs
