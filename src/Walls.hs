module Walls where
import System.Console.ANSI
import System.IO (stdout)

height = 8
width = 18

walls = [(x, y) | x <- [0..width], y <- [0, height]] ++ [(x, y) | x <- [0,width], y <- [1..height - 1]] ++ [(x, y) | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

wallSymboll = "█"

takeX :: (Int, Int) -> Int
takeX (x, y) = x

takeY :: (Int, Int) -> Int
takeY (x, y) = y

movePointer :: Int -> Int -> IO()
movePointer x y = setCursorPosition y x

displayWalls :: [(Int, Int)] -> IO()
displayWalls [] = return ()
displayWalls (h:hs) = do
    movePointer (takeX h) (takeY h)
    putStr wallSymboll
    displayWalls hs

isWall :: (Int, Int) -> Bool
isWall pos = pos `elem` walls
