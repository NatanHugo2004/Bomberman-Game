module Main (main) where

import System.IO
import System.Random
import System.Console.ANSI
import Structures
import GameLoop 
import Map
import Menu
import Timer
import Data.IORef
import Control.Concurrent

main :: IO ()
main = do
    let gameConfigs = GameConfigs 8 18 120 
    menu (width gameConfigs) ((height gameConfigs) + 1)
    hFlush stdout
    escolha <- getLine
    setSGR [Reset]
    if escolha == "1" then
        startGame gameConfigs
    else do
        menuExit ((height gameConfigs) + 1)

startGame :: GameConfigs -> IO ()
startGame gameConfigs = do
    hideCursor
    tempoRef <- newIORef (timerGamer gameConfigs) 
    startTimer tempoRef
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    gen <- newStdGen
    initialMap <- newIORef (createMap (height gameConfigs) (width gameConfigs) gen)
    _ <- forkIO $ updateBombTimers initialMap
    gameLoop initialMap gameConfigs tempoRef
    
