import Genetic
import Graphics.Gloss

generations = 400
seed = 40

main = do
    pointList <- fmap extractPoints $ readFile "data.txt"
    let final = finalPath pointList generations seed
    print final
    
extractPoints :: String -> Pointlist
extractPoints pointStr = map read broken
    where broken = lines pointStr

linesToDraw :: Maybe Pointlist -> Maybe [(Point, Point)]
linesToDraw Nothing = Nothing
linesToDraw ptList  = zip ptList infOffset
    where infOffset = drop 1 (cycle ptList)
