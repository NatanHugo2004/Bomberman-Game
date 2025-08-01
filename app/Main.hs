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
import Control.Concurrent (threadDelay)

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
        setSGR [Reset]
        showCursor
        exitSuccess
        

startGame :: IO ()
startGame = do
    hideCursor
    tempoRef <- newIORef 5
    let gameConfigs = GameConfigs 8 18
    startTimer tempoRef
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let initialMap = createMap (height gameConfigs) (width gameConfigs)
    gameLoop initialMap gameConfigs tempoRef
    

