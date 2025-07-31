module GameLoop where

--TODO: VERIFICAR SE PRECISA MESMO IMPORTAR STRUCTURES

import Structures
import System.IO
import Map 
import Display

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
