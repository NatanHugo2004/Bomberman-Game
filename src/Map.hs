module Map where

import Structures
import System.Random.Shuffle
import System.Random (StdGen) 

charToDirection :: Char -> (Point -> Point)
charToDirection c = case c of
'a' -> move (-1) 0
'd' -> move 1    0
'w' -> move 0    (-1)
's' -> move 0    1
_   -> id
    where
        move x y = \p -> createPoint ((takeX p) + x) ((takeY p) + y)

movePlayer :: (Point -> Point) -> Point -> Point
movePlayer direction player = direction player

plantBomb:: Point -> Int -> Bomb
plantBomb position timer = Bomb{
bombPosition = position,
timer = timer}

updateBomb:: Bomb -> Bomb
updateBomb bomb = Bomb (bombPosition bomb) ((timer bomb) -1)

explodeBombs :: Map -> [Bomb] -> Map
explodeBombs mapa [] = mapa
explodeBombs mapa (b:bs) = explodeBombs mapaAtualizado bs
  where
    pos = bombPosition b
    raio = 1
    raioBomba =
        [createPoint (takeX pos + dx) (takeY pos + dy) |
            dx <- [-raio..raio],
            dy <- [-raio..raio],
            abs dx + abs dy <= raio]
    
    pontosAfetados = filter(`notElem` walls mapa) raioBomba
    explosion = createExplosion (pontosAfetados)
    boxesRestantes = filter (`notElem` pontosAfetados) (boxes mapa)
    mapaAtualizado = mapa { boxes = boxesRestantes, explosions = explosion: explosions mapa}

createExplosion :: [Point] -> Explosion
createExplosion points = Explosion (points) (1)

updateExplosion :: Explosion -> Explosion
updateExplosion explosion = Explosion (explosionPosition explosion) ((time explosion) -1)


getExplosionsPoints :: [Explosion] -> [Point] -> [Point]
getExplosionsPoints [] points = points
getExplosionsPoints (j:js) points = getExplosionsPoints js (points ++ (explosionPosition j))


isDead :: Point -> [Explosion] -> Bool
isDead playerPosition explosions = playerPosition `elem` (getExplosionsPoints explosions []) 

isValidPlayerPosition :: Map -> Point -> Bool
isValidPlayerPosition map newPosition = not ((isWall newPosition (walls map)) || (isBox newPosition (boxes map)) || (isBomb newPosition (bombs map)) )

createWalls :: Int -> Int -> [Point]
createWalls height width =  [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                            [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                            [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

isWall :: Point -> [Point] -> Bool
isWall point walls = point `elem` walls

isBomb:: Point -> [Bomb] -> Bool
isBomb point bombs = point `elem` (map bombPosition bombs)

createBoxes :: Int -> Int -> [Point] -> Point -> StdGen -> [Point]
createBoxes height width walls player gen = 
  let
    allPoints = [createPoint x y | x <- [1..width-1], y <- [1..height-1]]
    validPoints = filter (\p -> not (p `elem` walls || p `elem` (neighbors player))) allPoints
    shuffled = shuffle' validPoints (length validPoints) gen
    boxesAmount = ceiling (0.70 * fromIntegral (length validPoints) :: Double)
  in take boxesAmount shuffled

isBox :: Point -> [Point] -> Bool
isBox point boxes = point `elem` boxes

createMap :: Int -> Int -> StdGen -> Map
createMap height width gen = Map walls boxes player [] []
    where
        walls = createWalls height width
        boxes = createBoxes height width walls player gen
        player = createPoint 1 1

updateMap :: Map -> Char -> Map
updateMap mapa ' ' =
    Map (walls mapa)
        (boxes mapa)
        (player mapa)
        (newBomb : bombs mapa)
        (explosions mapa)
  where
    playerPosition = player mapa
    newBomb = plantBomb playerPosition 3
        

updateMap mapa input = 
    Map (walls mapa) 
        (boxes mapa)
        (if (isValidPlayerPosition mapa newPlayerPosition) then
                newPlayerPosition 
            else 
                (player mapa)) 
       (bombs mapa)
       (explosions mapa)

    where 
        direction = charToDirection input
        newPlayerPosition = movePlayer direction (player mapa)
