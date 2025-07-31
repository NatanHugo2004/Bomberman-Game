module Main (main) where

import Structures
import GameLoop
import Map 
import Menu

main :: IO ()
main = do
    startGame

startGame :: IO ()
startGame = do
    let gameConfigs = GameConfigs 8 18
    let initialMap = createMap (height gameConfigs) (width gameConfigs)
    gameLoop initialMap