-- Este módulo tem como objetivo administrar operações pertinentes a estrutura de mapa, definida no módulo mapa. Bem como todas as operações referentes aos campos de um mapa atual.
module Map where

import Structures
import Utils
import Bomb
import System.Random.Shuffle
import System.Random (StdGen) 

--Função responsável por captar um caractere e retornar uma função que recebe um ponto e retorna um ponto.
charToDirection :: Char -> (Point -> Point)
charToDirection c = case c of
    'a' -> move (-1) 0
    'd' -> move 1    0
    'w' -> move 0    (-1)
    's' -> move 0    1
    _   -> id
    where
        move x y = \p -> createPoint ((takeX p) + x) ((takeY p) + y)

--Função responsável por verificar a validade da posição do player. Recebe um mapa, um ponto e retorna um valor booleano, indicando se o ponto passado como parâmetro pode ser uma posição válido a um jogador, não sendo nem parede, nem caixa, nem bomba.
isValidPlayerPos :: Map -> Point -> Bool
isValidPlayerPos map newPosition = not ((isWall newPosition (walls map)) || 
                                        (isBox  newPosition (boxes map)) || 
                                        (isBomb newPosition (bombs map)))

--Função responsável por criar paredes, recebendo dois inteiros representando a altura e largura do mapa e retornando um conjunto de pontos, representando as paredes.
createWalls :: Int -> Int -> [Point]
createWalls height width = [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                           [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                           [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

--Função responsável por criar caixas destrutivas, recebendo dois inteiros representando a altura e largura do mapa e retornando um conjunto de pontos, representando as caixas destrutivas. 
createBoxes :: Int -> Int -> [Point] -> Point -> StdGen -> [Point]
createBoxes height width walls player gen = take boxesAmount shuffled
    where
        allPoints    = [createPoint x y | x <- [1..width-1], y <- [1..height-1]]
        isValidPoint = \p -> not (p `elem` walls || p `elem` (neighbors player))
        validPoints  = filter isValidPoint allPoints
        shuffled     = shuffle' validPoints (length validPoints) gen
        boxesAmount  = ceiling (0.70 * fromIntegral (length validPoints) :: Double)

--Função responsável por criar um mapa, recebendo dois inteiros representando a altura e a largura do mapa, um conjunto de números aleatórios utilizados para geração das caixas, e retornando um mapa.
createMap :: Int -> Int -> StdGen -> Map
createMap height width gen = Map walls boxes player [] []
    where
        walls  = createWalls height width
        boxes  = createBoxes height width walls player gen
        player = createPoint 1 1

--Função responsável por atualizar um mapa, como na programação funcional os estados não são alterados, a lógica por trás da updateMap é a de receber um mapa, um caractere(que será usado para mover o personagem) e gerar um novo mapa com as novas posições, bombas e explosões.
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

createKeyPosition :: Int -> Int -> [Point] -> Point -> StdGen -> Point
createKeyPosition height width walls player gen = head $ shuffle' validPoints (length validPoints) gen
  where
    allPoints = [createPoint x y | x <- [width `div` 2..width-1], y <- [height `div` 2..height-1]]
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

isIn :: Eq a => a -> [a] -> Bool
isIn = elem

isWall :: Point -> [Point] -> Bool
isWall = isIn

isBox :: Point -> [Point] -> Bool
isBox = isIn

isBomb:: Point -> [Bomb] -> Bool
isBomb point bombs = isIn point (map bombPosition bombs)