module Main (main) where

import System.IO 
import Structures
import Display
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

