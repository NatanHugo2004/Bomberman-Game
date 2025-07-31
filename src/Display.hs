module Display where

import Structures
import System.Console.ANSI
import System.IO

data Symboll = Wall | Box | Player 

symbollToChar :: Symboll -> Char
symbollToChar Wall   = '#'
symbollToChar Box    = '*'
symbollToChar Player = '@'

movePointer :: Int -> Int -> IO()
movePointer x y = setCursorPosition y x

displayWalls :: [Point] -> IO()
displayWalls [] = return ()
displayWalls (h:hs) = do
    movePointer (takeX h) (takeY h)
    putChar (symbollToChar Wall)
    displayWalls hs

displayPlayer :: Point -> IO()
displayPlayer point = do
    movePointer (takeX point) (takeY point)
    putChar (symbollToChar Player)

display :: Map -> IO()
display map = do
    clearScreen
    displayWalls (walls map)
    displayPlayer (player map)
    hFlush stdout
