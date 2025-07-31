module Main (main) where

import Structures
import GameLoop
import Map 

main :: IO ()
main = do
    startGame

startGame :: IO ()
startGame = do
    let gameConfigs = GameConfigs 8 18
    let initialMap = createMap (height gameConfigs) (width gameConfigs) (createPoint 1 1)
    gameLoop initialMap

