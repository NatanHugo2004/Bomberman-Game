module GameLoop where

import Structures
import System.IO
import Map 
import Display
import Menu
import Data.IORef
import Bomb
import System.Console.ANSI
import Control.Concurrent

gameLoop :: IORef Map -> GameConfigs -> IORef Int -> IO ()
gameLoop mapRef configs tempoRef = do
    map <- readIORef mapRef
    time <- readIORef tempoRef
    display map configs time
    threadDelay 1000 
    if (checkGameOver map time) then do 
        displayPoint (player map) S_playerDeath sgrPlayerDeath
        hFlush stdout
        gameOverScreen configs
    else do   
        inputAvailable <- hReady stdin 
        if inputAvailable then do
            input <- getChar
            if input == 'q' then 
                quitScreen configs
            else do
                let newMap = updateMap map input
                if canExitThroughDoor newMap (player newMap) then 
                    gameWinScreen configs
                else do
                    writeIORef mapRef newMap
                    gameLoop mapRef configs tempoRef
        else do
            gameLoop mapRef configs tempoRef
    where
        sgrPlayerDeath = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]

checkGameOver :: Map -> Int -> Bool
checkGameOver map time = time <= 0 || isDead (player map) (explosions map)
