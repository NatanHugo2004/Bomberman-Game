module Destructibles where
import System.Console.ANSI
import System.IO (stdout)
import Walls

destructibleSymbol :: String
destructibleSymbol = "▓"

destructibles :: [(Int, Int)]
destructibles =[(x, y) | x <- [3,4..width - 3], y <- [1,2,height - 2, height - 1], not (isWall (x, y))] ++ [(x, y) | x <- [1,2..width - 1], y <- [3,4..height - 3], not (isWall (x, y))]

displayDestructibles :: [(Int, Int)] -> IO ()
displayDestructibles [] = return ()
displayDestructibles (d:ds) = do
    movePointer (takeX d) (takeY d)
    setSGR [SetColor Foreground Dull Yellow]
    putStr destructibleSymbol
    displayDestructibles ds
    setSGR [Reset]

isDestructibles :: (Int, Int) -> Bool
IsDestructibles pos = pos `elem` destructibles
