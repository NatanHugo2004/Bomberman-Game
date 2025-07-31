module Map where
import System.Console.ANSI
import System.IO (stdout)
import Walls 
import Destructibles

display :: IO()
display = do
    clearScreen
    displayWalls walls
    displayDestructibles destructibles
