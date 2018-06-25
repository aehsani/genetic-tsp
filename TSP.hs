import Genetic

generations :: Int
generations = 400

seed :: Int
seed = 30

main = do
    pointList <- fmap extractPoints $ readFile "data.txt"
    let final = finalPath pointList generations seed
    print final
    
extractPoints :: String -> [(Int, Int)]
extractPoints pointStr = map read broken
    where broken = lines pointStr
