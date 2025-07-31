module Walls where
import System.Console.ANSI
import System.IO

newtype Point = Point (Int, Int) deriving (Eq)

data Map = Map { 
        walls :: [Point],
        player :: Point
    }

data GameConfigs = GameConfigs {
        height :: Int,
        width :: Int
    }

data Symboll = Wall | Box | Player 

symbollToChar :: Symboll -> Char
symbollToChar Wall   = '#'
symbollToChar Box    = '*'
symbollToChar Player = '@'

createMap :: Int -> Int -> Point -> Map
createMap height width point = Map walls point
    where
        walls = [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

createPoint :: Int -> Int -> Point
createPoint x y = (Point (x, y))

takeX :: Point -> Int
takeX (Point(x, y)) = x

takeY :: Point -> Int
takeY (Point(x, y)) = y

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
