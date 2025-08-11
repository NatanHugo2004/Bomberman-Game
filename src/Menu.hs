module Menu where

import Utils
import Structures
import Display
import Control.Concurrent
import System.Console.ANSI
import System.IO (hFlush,stdout)
import System.Exit

retangulo:: Int -> Int -> [Point]
retangulo width height = [Point (x, y) | x <- [0..width], y <- [0, height]] ++ [Point (x, y) | x <- [0,width], y <- [1..height - 1]]

verificaCaractere :: Point -> Int -> Int-> String
verificaCaractere (Point (x,y)) width height
 | (x, y) `elem` corners = "+"
 | x == 0 || x == width  = "|"
 | y == 0 || y == height = "-"
 | otherwise             = ""
 where
    corners = [(0, 0), (0, height), (width, 0), (width, height)] 

displayRetangulo :: [Point] -> Int -> Int -> IO()
displayRetangulo [] width height  = return ()
displayRetangulo (h:hs) width height = do
    movePointer (takeX h) (takeY h)
    putStr(verificaCaractere h width height)
    displayRetangulo hs width height

menu :: Int -> Int -> IO()
menu width height = do
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
    displayRetangulo (retangulo width height) width height
    movePointer ((width `div` 2) - 4) 3
    setSGR [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink,SetColor Foreground Vivid White]
    putStr("BOMBERMAN")
    setSGR [SetConsoleIntensity NormalIntensity, SetBlinkSpeed NoBlink, SetColor Foreground Vivid White]
    movePointer ((width `div` 2) - 4) 5
    putStr("[1] PLAY")
    movePointer ((width `div` 2) - 4) 6
    setSGR [SetConsoleIntensity BoldIntensity, SetBlinkSpeed NoBlink, SetColor Foreground Dull Red]
    putStr("[2] EXIT")
    setSGR[Reset]
    movePointer 9 11

instructions :: Int -> Int -> IO()
instructions width height = do
    clearScreen
    movePointer (width) 2
    setSGR[SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink]
    putStr"+---INSTRUCTIONS OF THE GAME---+"
    setSGR[Reset]
    movePointer (width - 5) 4
    putStr"MOVEMENT: Use" 
    setSGR[SetConsoleIntensity BoldIntensity]
    putStr" WASD "
    setSGR[Reset]
    putStr"to move your character"
    movePointer 2 5
    putStr"BOMB: Use" 
    setSGR[SetConsoleIntensity BoldIntensity]
    putStr" SPACE "
    setSGR[Reset]
    putStr"to place a" 
    setSGR[SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink]
    putStr" bomb " 
    setSGR[Reset]
    putStr"- DONT STAY IN THE" 
    setSGR[SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    putStr" EXPLOSION AREA "
    setSGR[Reset]
    movePointer 11 6
    putStr"Destroy walls to find the" 
    setSGR[SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr" KEY " 
    setSGR[Reset]
    putStr"to escape -->"
    setSGR[SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr" ⚷"
    setSGR[Reset]
    movePointer 14 7
    putStr"Run to the"
    setSGR[SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    putStr" DOOR "
    setSGR[Reset]
    putStr"after get the key -->"
    setSGR[SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    putStr" በ"
    setSGR[Reset]
    movePointer 18 8
    putStr"Press"
    setSGR[SetConsoleIntensity BoldIntensity]
    putStr" Q "
    setSGR[Reset]
    putStr"to quit during the game"
    movePointer 17 10
    setSGR[SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink]
    putStr"PRESS ENTER TO CONTINUE TO THE GAME\n\n"
    setSGR[Reset]

menuExit :: Int -> IO()
menuExit height = do 
	clearScreen
	hideCursor
	setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
	setCursorPosition (height `div` 2 - 2) 0
	putStrLn "Not in the mood to play?"
	threadDelay 1000000
	putStrLn "Come back soon and have fun!!\n"
	threadDelay 500000
	setSGR [Reset]
	showCursor
	exitSuccess

symbolLoop :: Int -> Char -> IO ()
symbolLoop 0 _ = putStrLn ""
symbolLoop n c = do
    putChar c
    hFlush stdout
    threadDelay 300000
    symbolLoop (n - 1) c

quitScreen :: GameConfigs -> IO ()
quitScreen configs = do
    clearScreen
    setCursorPosition ((height configs) `div` 2 - 2) 0
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    putStr "QUITTING"
    symbolLoop 3 '.'
    threadDelay 300000
    putStr"\n"
    setSGR [Reset]
    showCursor

gameWinScreen :: GameConfigs -> IO ()
gameWinScreen configs = do
    clearScreen
    setCursorPosition ((height configs) `div` 2 - 2) 0
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
    putStr"OMGG :OO\n"
    threadDelay 1000000
    putStr"YOU FOUND THE WAY OUT\n"
    threadDelay 1000000
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "YOU WIN! "
    threadDelay 1000000
    putStr "YOU ESCAPED"
    symbolLoop 3 '!'
    threadDelay 1000000
    setSGR [Reset]
    mapM_ (\_ -> firework) [1..5]
    showCursor

gameOverScreen :: GameConfigs -> IO ()
gameOverScreen configs = do
    threadDelay 1000000
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    setCursorPosition ((height configs) `div` 2 - 2) 0
    putStrLn "..."
    threadDelay 1000000
    clearLine
    putStrLn "GAME OVER\nYOU LOSE\n"
    threadDelay 1300000
    clearScreen
    setCursorPosition ((height configs) `div` 2 - 2) 0
    putStr "IDIOT HAHAHA"
    symbolLoop 5 '!' 
    threadDelay 1000000
    setSGR [Reset]
    showCursor
    clearScreen

playAgainScreen :: GameConfigs -> IO()
playAgainScreen configs = do
    hideCursor
    clearScreen
    movePointer ((height configs) `div` 2 - 3) ((width configs) `div` 2 - 8)
    setSGR[SetBlinkSpeed SlowBlink, SetColor Foreground Vivid White]
    putStr"+---DO YOU WANT TO PLAY AGAIN?---+"
    threadDelay 500000
    setSGR[Reset]
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
    movePointer ((height configs) `div` 2 + 1) ((width configs) `div` 2 - 6)
    putStr("[1] PLAY AGAIN")
    movePointer ((height configs) `div` 2 + 18) ((width configs) `div` 2 - 6)
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Red]
    putStr("[2] EXIT\n\n")

byeScreen :: GameConfigs -> IO()
byeScreen configs = do
    clearScreen
    movePointer 0 ((height configs) `div` 2 - 2)
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
    putStr"Dont want to play anymore?\n"
    threadDelay 1000000
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
    putStr"Bye, See you again xD\n\n"
    threadDelay 1000000
    setSGR[Reset]



    

