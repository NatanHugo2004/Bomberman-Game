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
import System.Random
import Timer

gameLoop :: IORef Map -> GameConfigs -> IORef Int -> ThreadId -> ThreadId -> IO ()
gameLoop mapRef configs tempoRef bombTID timerID = do
    map <- readIORef mapRef
    time <- readIORef tempoRef
    display map configs time
    threadDelay 1000 
    if (checkGameOver map time) then do 
        displayPoint (player map) S_playerDeath sgrPlayerDeath
        hFlush stdout
        gameOverScreen configs
        playAgainScreen configs
        input <- getChar
        if input == '1' then do
            stopThreads [bombTID, timerID]
            startGame configs
        else do
            byeScreen configs
    else do   
        inputAvailable <- hReady stdin 
        if inputAvailable then do
            input <- getChar
            if input == 'q' then 
                quitScreen configs
            else do
                let newMap = updateMap map input
                if canExitThroughDoor newMap (player newMap) then do
                    gameWinScreen configs
                    playAgainScreen configs
                    playInput <- getChar
                    if playInput == '1' then do
                        stopThreads [bombTID, timerID]
                        startGame configs
                    else do
                        byeScreen configs
                else do
                    writeIORef mapRef newMap
                    gameLoop mapRef configs tempoRef bombTID timerID
        else do
            gameLoop mapRef configs tempoRef bombTID timerID
    where
        sgrPlayerDeath = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]

checkGameOver :: Map -> Int -> Bool
checkGameOver map time = time <= 0 || isDead (player map) (explosions map)

startGame :: GameConfigs -> IO ()
startGame gameConfigs = do
    hideCursor
    tempoRef <- newIORef (timerGamer gameConfigs) 
    timerID <- startTimer tempoRef
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    gen <- newStdGen
    initialMap <- newIORef (createMap (height gameConfigs) (width gameConfigs) gen)
    bombTID <- startBombTimers initialMap
    gameLoop initialMap gameConfigs tempoRef bombTID timerID