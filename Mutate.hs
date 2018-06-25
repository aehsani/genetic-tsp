module Mutate
( reproduce
) where

import System.Random

type Ordrep = [Int]
type Canonic = [Int]
type Tour = [Int]


reproduce []      numNew seed = []
reproduce currGen numNew seed = currGen ++ children
    where children = crossGen currGen numNew stdGen
          stdGen = mkStdGen seed

crossGen []      numNew stdGen = []
crossGen currGen numNew stdGen
        | numNew <= 0          = []
crossGen currGen numNew stdGen = x:crossGen currGen (numNew-1) stdGen''
    where (x, stdGen'')   = crossTwo p q stdGen'
          (p, q, stdGen') = selectTwo currGen stdGen

selectTwo []  stdGen = error "No species to choose from"
selectTwo gen stdGen = (p, q, stdGen')
    where p = gen !! pos
          q = gen !! fst (randomR (0,bound) stdGen')
          (pos, stdGen') = randomR (0,bound) stdGen
          bound = length gen - 1

crossTwo p q stdGen = (getBackTour newOrdrep, stdGen')
    where (newOrdrep, stdGen') = switchOrd pOrd qOrd stdGen
          pOrd = getOrdrep p
          qOrd = getOrdrep q

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
