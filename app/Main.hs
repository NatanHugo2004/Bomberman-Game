module Main (main) where

import System.IO 
import Walls

main :: IO ()
main = do
    startGame

startGame :: IO ()
startGame = do
    let gameConfigs = GameConfigs 8 18
    let initialMap = createMap (height gameConfigs) (width gameConfigs) (createPoint 1 1)
    gameLoop initialMap

-- TODO: É INTERESSANTE FAZER A VERIFICAÇÃO SE RECEBEU UMA ENTRADA
-- VÁLIDA, CASO SIM, GERA UM MAPA NOVO, CASO NÃO REAPROVEITA O MESMO
-- MAPA

gameLoop :: Map -> IO ()
gameLoop map = do
    display map
    movePointer 0 10
    hFlush stdout
    input <- getChar
    let newMap = updateMap map input
    if (input == 'q')
        then return ()
        else gameLoop newMap

-- TODO: VERIFICAR SE DÁ PRA FAZER ALGO MAIS BONITO

charToDirection :: Char -> (Point -> Point)
charToDirection 'a' = \p -> createPoint ((takeX p) - 1) (takeY p)
charToDirection 'd' = \p -> createPoint ((takeX p) + 1) (takeY p)
charToDirection 'w' = \p -> createPoint (takeX p) ((takeY p) - 1)
charToDirection 's' = \p -> createPoint (takeX p) ((takeY p) + 1)
charToDirection c = \p -> createPoint (takeX p) (takeY p)

updateMap :: Map -> Char -> Map
updateMap map input = 
    Map (walls map) 
        (if (isValidPlayerPosition map newPlayerPosition) then
            newPlayerPosition 
        else 
            (player map)) 
    where 
        direction = charToDirection input
        newPlayerPosition = movePlayer direction (player map)

movePlayer :: (Point -> Point) -> Point -> Point
movePlayer direction player = direction player

isValidPlayerPosition :: Map -> Point -> Bool
isValidPlayerPosition map newPosition = not (elem newPosition (walls map))
