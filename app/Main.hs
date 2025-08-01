module Main (main) where

import System.IO
import Structures
import GameLoop
import Map
import Menu
import System.Exit
import System.Console.ANSI

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
    else exitSuccess

startGame :: IO ()
startGame = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let gameConfigs = GameConfigs 8 18
    let initialMap = createMap (height gameConfigs) (width gameConfigs)
    gameLoop initialMap
