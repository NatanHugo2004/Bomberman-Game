module Display where

import Structures
import System.Console.ANSI
import System.IO

data Symboll = Wall | Box | Player 

symbollToChar :: Symboll -> Char
symbollToChar Wall   = '█'
symbollToChar Box    = '▓'
symbollToChar Player = '@'

movePointer :: Int -> Int -> IO()
movePointer x y = setCursorPosition y x

displayWalls :: [Point] -> IO()
displayWalls [] = return ()
displayWalls (w:ws) = do
    movePointer (takeX w) (takeY w)
    putChar (symbollToChar Wall)
    displayWalls ws

displayBoxes :: [Point] -> IO ()
displayBoxes [] = return ()
displayBoxes (b:bs) = do
    movePointer (takeX b) (takeY b)
    setSGR [SetColor Foreground Dull Yellow]
    putChar (symbollToChar Box)
    displayBoxes bs
    setSGR [Reset]

displayPlayer :: Point -> IO()
displayPlayer point = do
    movePointer (takeX point) (takeY point)
    putChar (symbollToChar Player)

display :: Map -> IO()
display map = do
    clearScreen
    displayWalls (walls map)
    displayBoxes (boxes map)
    displayPlayer (player map)
    hFlush stdout
