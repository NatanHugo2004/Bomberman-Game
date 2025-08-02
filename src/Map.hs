module Map where

import Structures 

-- TODO: VERIFICAR SE DÁ PRA FAZER ALGO MAIS BONITO
charToDirection :: Char -> (Point -> Point)
charToDirection 'a' = \p -> createPoint ((takeX p) - 1) (takeY p)
charToDirection 'd' = \p -> createPoint ((takeX p) + 1) (takeY p)
charToDirection 'w' = \p -> createPoint (takeX p) ((takeY p) - 1)
charToDirection 's' = \p -> createPoint (takeX p) ((takeY p) + 1)
charToDirection c = \p -> createPoint (takeX p) (takeY p)

movePlayer :: (Point -> Point) -> Point -> Point
movePlayer direction player = direction player

plantBomb:: Point -> Int -> Bomb
plantBomb position timer = Bomb{
bombPosition = position,
timer = timer}

updateBomb:: Bomb -> Bomb
updateBomb bomb = Bomb (bombPosition bomb) ((timer bomb) -1)

activatedBomb:: Bomb -> Bool
activatedBomb bomb = (timer bomb) > 0




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

createBoxes :: Int -> Int -> [Point] -> [Point]
createBoxes height width walls = [createPoint x y | x <- [3,4..width - 3], y <- [1,2,height - 2, height - 1], not (isWall (createPoint x y) walls)] ++ 
                                 [createPoint x y | x <- [1,2..width - 1], y <- [3,4..height - 3], not (isWall (createPoint x y) walls)]

isBox :: Point -> [Point] -> Bool
isBox point boxes = point `elem` boxes

createMap :: Int -> Int -> Map
createMap height width = Map walls boxes player []
    where
        walls = createWalls height width
        boxes = createBoxes height width walls
        player = createPoint 1 1

updateMap :: Map -> Char -> Map
updateMap mapa ' ' =
    Map (walls mapa)
        (boxes mapa)
        (player mapa)
        (newBomb : bombs mapa)
  where
    playerPosition = player mapa
    newBomb = plantBomb playerPosition 5
        

updateMap mapa input = 
    Map (walls mapa) 
        (boxes mapa)
        (if (isValidPlayerPosition mapa newPlayerPosition) then
                newPlayerPosition 
            else 
                (player mapa)) 
       (bombs mapa)

    where 
        direction = charToDirection input
        newPlayerPosition = movePlayer direction (player mapa)
