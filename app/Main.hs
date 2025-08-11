module Main (main) where

import System.IO
import System.Console.ANSI
import Structures
import GameLoop (startGame)
import Menu

main :: IO ()
main = do
    hideCursor
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let gameConfigs = GameConfigs 8 18 120 
    menu (width gameConfigs) ((height gameConfigs) + 1)
    hFlush stdout
    escolha <- getInput
    setSGR [Reset]
    if escolha == '1' then do
        instructions (width gameConfigs) (height gameConfigs)
        hFlush stdout
        input <- getEnter
        if input == '\n' then
            startGame gameConfigs
        else
            return()
    else do
        menuExit ((height gameConfigs) + 1)
        showCursor


    
