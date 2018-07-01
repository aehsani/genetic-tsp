import Genetic
import Graphics.Gloss

generations = 10000
seed = 20

main = do
    pointList <- fmap extractPoints $ readFile "data.txt"
    let scale = getScale pointList
    let final = finalPath pointList generations seed
    case final of Nothing -> return ()
                  Just x  -> drawStuff x scale

getScale :: [(Int, Int)] -> Float
getScale ptList = (max xHalf yHalf) / (actual+10)
    where (xHalf, yHalf) = (fromIntegral xSize/2, fromIntegral ySize/2)
          actual = maximum $ fmap f ptList
          f = \(x,y) -> fromIntegral $ max (abs x) (abs y)

extractPoints :: String -> [(Int, Int)]
extractPoints pointStr = map read broken
    where broken = lines pointStr





ySize = 1500
xSize = 2400

window :: Display
window = InWindow "TSP Path" (xSize, ySize) (10, 10)

background :: Color
background = black

drawStuff :: [(Int, Int)] -> Float -> IO ()
drawStuff pts scale = display window background drawing
    where drawing = pictures $ lnsDraw:ptsDraw
          ptsDraw = picPts pts'
          lnsDraw = color red $ lineLoop pts'
          pts' = map (scalePt scale) pts

picPts :: [(Float, Float)] -> [Picture]
picPts pts = map trans pts
    where trans (x, y) = translate x y pt 
          pt = color red $ circleSolid 20
    
scalePt :: Float -> (Int, Int) -> (Float, Float)
scalePt scale (x, y) = (x', y')
    where x' = scale*fromIntegral x
          y' = scale*fromIntegral y

