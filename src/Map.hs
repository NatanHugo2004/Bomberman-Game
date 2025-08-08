module Map where

import Structures
import Utils
import Bomb
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

isValidPlayerPos :: Map -> Point -> Bool
isValidPlayerPos map newPosition = not ((isWall newPosition (walls map)) || 
                                        (isBox  newPosition (boxes map)) || 
                                        (isBomb newPosition (bombs map)))

createWalls :: Int -> Int -> [Point]
createWalls height width = [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                           [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                           [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

createBoxes :: Int -> Int -> [Point] -> Point -> StdGen -> [Point]
createBoxes height width walls player gen = take boxesAmount shuffled
    where
        allPoints    = [createPoint x y | x <- [1..width-1], y <- [1..height-1]]
        isValidPoint = \p -> not (p `elem` walls || p `elem` (neighbors player))
        validPoints  = filter isValidPoint allPoints
        shuffled     = shuffle' validPoints (length validPoints) gen
        boxesAmount  = ceiling (0.70 * fromIntegral (length validPoints) :: Double)

createMap :: Int -> Int -> StdGen -> Map
createMap height width gen = Map walls boxes player [] [] door (Just keyPosition)  False
  where
    walls       = createWalls height width
    player      = createPoint 1 1
    door        = createPoint (width - 1) (height - 1) 
    keyPosition = createKeyPosition height width walls player gen
    boxes       = createBoxes height width walls player gen

createKeyPosition :: Int -> Int -> [Point] -> Point -> StdGen -> Point
createKeyPosition height width walls player gen = head $ shuffle' validPoints (length validPoints) gen
  where
    -- Pega todos os pontos válidos na parte oposta do mapa (longe do jogador)
    allPoints = [createPoint x y | x <- [width `div` 2..width-1], y <- [height `div` 2..height-1]]
    -- Um ponto é válido se não for uma parede e não for o ponto inicial do jogador
    isValidPoint p = not (p `elem` walls || p `elem` (neighbors player))
    validPoints  = filter isValidPoint allPoints   

collectKey :: Map -> Point -> Map
collectKey mapa newPlayerPos = 
  case key mapa of
    Just k -> 
      if newPlayerPos == k
        then mapa { player = newPlayerPos, hasKey = True, key = Nothing }
        else mapa { player = newPlayerPos }
    Nothing -> mapa { player = newPlayerPos }  

-- Função que verifica se o jogador está na porta e já tem a chave
canExitThroughDoor :: Map -> Point -> Bool
canExitThroughDoor mapa newPlayerPos =
  newPlayerPos == door mapa && hasKey mapa

updateMap :: Map -> Char -> Map
updateMap mapa input
  | input == ' ' = mapa {bombs = addBomb newBomb (bombs mapa)}
  | otherwise    =
    let
      playerPos    = player mapa
      direction    = charToDirection input
      newPlayerPos = direction playerPos
    in
      if isValidPlayerPos mapa newPlayerPos
        then collectKey mapa newPlayerPos
        else mapa
  where
    newBomb = plantBomb (player mapa) 3

isIn :: Eq a => a -> [a] -> Bool
isIn = elem

isWall :: Point -> [Point] -> Bool
isWall = isIn

isBox :: Point -> [Point] -> Bool
isBox = isIn

isBomb:: Point -> [Bomb] -> Bool
isBomb point bombs = isIn point (map bombPosition bombs)
