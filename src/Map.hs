module Map where

import Structures
import System.Random.Shuffle
import System.Random (StdGen) 
import Bomb

charToDirection :: Char -> (Point -> Point)
charToDirection c = case c of
    'a' -> move (-1) 0
    'd' -> move 1    0
    'w' -> move 0    (-1)
    's' -> move 0    1
    _   -> id
    where
        move x y = \p -> createPoint ((takeX p) + x) ((takeY p) + y)

isValidPlayerPos :: Map -> Point -> Bool
isValidPlayerPos map newPosition = not ((isWall newPosition (walls map)) || 
                                        (isBox  newPosition (boxes map)) || 
                                        (isBomb newPosition (bombs map)))

createWalls :: Int -> Int -> [Point]
createWalls height width =  [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                            [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                            [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

isWall :: Point -> [Point] -> Bool
isWall point walls = point `elem` walls

createBoxes :: Int -> Int -> [Point] -> Point -> StdGen -> [Point]
createBoxes height width walls player gen = take boxesAmount shuffled
    where
        allPoints    = [createPoint x y | x <- [1..width-1], y <- [1..height-1]]
        isValidPoint = \p -> not (p `elem` wall || p `elem` (neighbors player))
        validPoints  = filter isValidPoint allPoints
        shuffled     = shuffle' validPoints (length validPoints) gen
        boxesAmount  = ceiling (0.70 * fromIntegral (length validPoints) :: Double)

isBox :: Point -> [Point] -> Bool
isBox point boxes = point `elem` boxes

createMap :: Int -> Int -> StdGen -> Map
createMap height width gen = Map walls boxes player [] []
    where
        walls = createWalls height width
        boxes = createBoxes height width walls player gen
        player = createPoint 1 1

updateMap :: Map -> Char -> Map
updateMap map input
 | input == ' '                      = map {bombs = addBomb newBomb (bombs map)}
 | isValidPlayerPos map newPlayerPos = map {player = newPlayerPos}
 | otherwise                         = map
    where
        playerPos    = player map
        newBomb      = plantBomb playerPos 3
        direction    = charToDirection input
        newPlayerPos = direction playerPos

