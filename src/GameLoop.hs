module GameLoop where

import Structures
import System.IO
import Map 
import Display
import Data.IORef
import System.Console.ANSI
import Control.Concurrent


gameLoop :: IORef Map -> GameConfigs -> IORef Int -> IO ()
gameLoop mapRef configs tempoRef = do
    clearScreen
    mapa <- readIORef mapRef
    display mapa

    tempo <- readIORef tempoRef
    setCursorPosition ((height configs) + 2) 0
    clearLine
    let total = 120
    let filled = replicate (tempo * 13 `div` total) '■'
    let empty = replicate (13 - length filled) ' '
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "══ Remaining Time ══\n"
    setSGR [Reset]
    putStr $ "│" ++ filled ++ empty ++ "│ "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr $ show tempo ++ "s "
    setSGR [Reset]
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "\n════════════════════\n"
    setSGR [Reset]
    hFlush stdout

    inputAvailable <- hReady stdin

    if inputAvailable then do
        input <- getChar
        if input == 'q' then do
            setCursorPosition ((height configs) `div` 2 - 2) 0
            clearScreen
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
            putStr "QUITTING"
            let loopPonto :: Int -> IO ()
                loopPonto 0 = putStrLn"\n"
                loopPonto n = do
                    putChar '.'
                    hFlush stdout
                    threadDelay 300000 
                    loopPonto (n - 1)
            loopPonto 3
            threadDelay 300000
            setSGR [Reset]
            showCursor
        else do
            let newMap = updateMap mapa input
            writeIORef mapRef newMap

            if tempo < 0
                then do
                    setCursorPosition ((height configs) + 2) 0
                    clearLine
                    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                    putStrLn "..."
                    threadDelay 1000000
                    clearScreen
                    setCursorPosition ((height configs) `div` 2 - 2) 0
                    putStrLn "..."
                    threadDelay 1000000
                    clearLine
                    putStrLn "GAME OVER\nYOU LOSE\n"
                    threadDelay 1300000
                    clearScreen
                    setCursorPosition ((height configs) `div` 2 - 2) 0
                    putStr "IDIOT HAHAHA"
                    let loopExclamacao :: Int -> IO ()
                        loopExclamacao 0 = putStrLn"\n"
                        loopExclamacao n = do
                            putChar '!'
                            hFlush stdout
                            threadDelay 300000 
                            loopExclamacao (n - 1)
                    loopExclamacao 8  
                    setSGR [Reset]
                    showCursor
                else gameLoop mapRef configs tempoRef
    else do
        threadDelay 100000 
        updatedTime <- readIORef tempoRef
        if updatedTime <= 0
            then do
                setCursorPosition ((height configs) + 2) 0
                clearLine
                setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                putStrLn "..."
                threadDelay 1000000
                clearScreen
                setCursorPosition ((height configs) `div` 2 - 2) 0
                putStrLn "..."
                threadDelay 1000000
                clearLine
                putStrLn "GAME OVER\nYOU LOSE\n"
                threadDelay 1300000
                clearScreen
                setCursorPosition ((height configs) `div` 2 - 2) 0
                putStr "IDIOT HAHAHA"
                let loopExclamacao :: Int -> IO ()
                    loopExclamacao 0 = putStrLn"\n"
                    loopExclamacao n = do
                        putChar '!'
                        hFlush stdout
                        threadDelay 300000 
                        loopExclamacao (n - 1)
                loopExclamacao 8  
                setSGR [Reset]
                showCursor
            else gameLoop mapRef configs tempoRef
