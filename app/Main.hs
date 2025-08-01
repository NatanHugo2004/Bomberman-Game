module Main (main) where

import System.IO
import Structures
import GameLoop 
import Map
import Menu
import System.Exit
import System.Console.ANSI
import Timer
import Data.IORef
import Control.Concurrent
import System.Random

main :: IO ()
main = do
    menu 18 9
    movePointer 5 10
    putStr "ESCOLHA:"
    movePointer 9 11
    hFlush stdout
    escolha <- getLine
    setSGR [Reset]
    if escolha == "1" then
       startGame
    else do
        clearScreen
        hideCursor
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
        setCursorPosition (9 `div` 2 - 2) 0
        putStrLn "Not in the mood to play?"
        threadDelay 1000000
        putStrLn "Come back soon and have fun!!\n"
        threadDelay 500000
        setSGR [Reset]
        showCursor
        exitSuccess
        

startGame :: IO ()
startGame = do
    hideCursor
    tempoRef <- newIORef 120
    let gameConfigs = GameConfigs 8 18
    startTimer tempoRef
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    gen <- newStdGen
    initialMap <- newIORef (createMap (height gameConfigs) (width gameConfigs) gen)
    _ <- forkIO $ updateBombTimers initialMap
    gameLoop initialMap gameConfigs tempoRef
    
