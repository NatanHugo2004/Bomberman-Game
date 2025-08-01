module GameLoop where

import Structures
import System.IO
import Map 
import Display


gameLoop :: Map -> IO ()
gameLoop map = do
    -- A cada ciclo, processamos as bombas e criamos um novo mapa.
    let mapWithProcessedBombs = processBombs map

    -- Exibimos o mapa atualizado.
    display mapWithProcessedBombs
    hFlush stdout
    
    -- Lemos a entrada se ela estiver pronta. 
    input <- getChar 
  

    -- Se a entrada for 'q', o loop para. Caso contrário, ele continua.
    if input == 'q'
        then return ()
        else do 
            let newMap = updateMap mapWithProcessedBombs input
            gameLoop newMap