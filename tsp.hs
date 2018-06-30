import Genetic

generations = 200
seed = 20

main = do
    pointList <- fmap extractPoints $ readFile "data.txt"
    let final = finalPath pointList generations seed
    print $ final
    
extractPoints :: String -> [(Int, Int)]
extractPoints pointStr = map read broken
    where broken = lines pointStr

--linesToDraw :: Maybe Pointlist -> Maybe [(Point, Point)]
--linesToDraw Nothing = Nothing
--linesToDraw ptList  = zip ptList infOffset
--    where infOffset = drop 1 (cycle ptList)
