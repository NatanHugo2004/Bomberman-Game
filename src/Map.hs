module Map where

import Structures 


charToDirection :: Char -> (Point -> Point)
charToDirection 'a' = \p -> createPoint ((takeX p) - 1) (takeY p)
charToDirection 'd' = \p -> createPoint ((takeX p) + 1) (takeY p)
charToDirection 'w' = \p -> createPoint (takeX p) ((takeY p) - 1)
charToDirection 's' = \p -> createPoint (takeX p) ((takeY p) + 1)
charToDirection c = \p -> createPoint (takeX p) (takeY p)
    

movePlayer :: (Point -> Point) -> Point -> Point
movePlayer direction player = direction player

isValidPlayerPosition :: Map -> Point -> Bool
isValidPlayerPosition map newPosition = not ((isWall newPosition (walls map)) || (isBox newPosition (boxes map)))

createWalls :: Int -> Int -> [Point]
createWalls height width =  [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                            [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                            [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

isWall :: Point -> [Point] -> Bool
isWall point walls = point `elem` walls

createBoxes :: Int -> Int -> [Point] -> [Point]
createBoxes height width walls = [createPoint x y | x <- [3,4..width - 3], y <- [1,2,height - 2, height - 1], not (isWall (createPoint x y) walls)] ++ 
                                 [createPoint x y | x <- [1,2..width - 1], y <- [3,4..height - 3], not (isWall (createPoint x y) walls)]

isBox :: Point -> [Point] -> Bool
isBox point boxes = point `elem` boxes

createMap :: Int -> Int -> Map
createMap height width = Map walls  boxes player []
    where
        walls = createWalls height width
        boxes = createBoxes height width walls
        player = createPoint 1 1

-- Adiciona uma bomba na posição do jogador
placeBomb :: Map -> Map
placeBomb map = 
    let newBomb = Bomb { bombPosition = player map, timer = 3}
    in map { bombs = newBomb : bombs map}

-- gera os pontos de explosão de uma bomba
explosionPoints :: Bomb -> [Point]
explosionPoints bomb =
    let (Point(x, y)) = bombPosition bomb
    in [Point(x, y), Point(x-1, y), Point(x+1, y), Point(x, y-1), Point(x, y+1)]

processBombs :: Map -> Map
processBombs currentMap =
    let allBombs = bombs currentMap
        bombsAfterTick = map (\b -> b { timer = timer b - 1}) allBombs
        bombsToExplode = filter (\b -> timer b <= 0) bombsAfterTick
        activeBombs = filter (\b -> timer b > 0) bombsAfterTick

        --calculando todos os pontos de explosão
        explodedPoints = concatMap explosionPoints bombsToExplode
        -- Filtramos os blocos destrutíveis que não estão nos pontos de explosão
        newBoxes = filter (\box -> not (box `elem` explodedPoints)) (boxes currentMap)
        -- TODO: Adicionar lógica para o jogador ser atingido pela explosão
    in currentMap {bombs = activeBombs, boxes = newBoxes}    


-- A função principaç que lida com todas as entradasdo usuário
updateMap :: Map -> Char -> Map
updateMap map input =
    case input of
        ' ' -> placeBomb map
        'q' -> map -- Saída
        _   -> updateMapWithMovement map input

-- Função auxiliar que processa a movimentação do jogador
updateMapWithMovement :: Map -> Char -> Map
updateMapWithMovement map input =
    let direction = charToDirection input
        newPlayerPosition = movePlayer direction (player map)
    in if isValidPlayerPosition map newPlayerPosition
        then map { player = newPlayerPosition}    
        else map 

        

--updateMap :: Map -> Char -> Map
--updateMap map input = 
  --  Map (walls map) 
    --    (boxes map)
      -- (if (isValidPlayerPosition map newPlayerPosition) then
         --   newPlayerPosition 
  --      else 
        --    (player map)) 
  --  where 
    --    direction = charToDirection input
      --  newPlayerPosition = movePlayer direction (player map)



