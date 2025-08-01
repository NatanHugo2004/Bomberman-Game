module Menu where
import System.Console.ANSI
import System.IO (hFlush,stdout)
import Structures

retangulo:: Int -> Int -> [Point]
retangulo width height = [Point (x, y) | x <- [0..width], y <- [0, height]] ++ [Point (x, y) | x <- [0,width], y <- [1..height - 1]]

verificaCaractere :: Point -> Int -> Int-> String
verificaCaractere (Point (n,m)) width height
 | n == 0 && m == 0 || n == 0 && m == height || n == width && m == 0 || n == width && m == height  = "+"
 | n == 0 || n == width   = "|"
 | m == 0 || m == height  = "-"

displayRetangulo :: [Point] -> Int -> Int -> IO()
displayRetangulo [] width height  = return ()
displayRetangulo (h:hs) width height = do
                        movePointer (takeX h) (takeY h)
                        putStr(verificaCaractere h width height)
                        displayRetangulo hs width height

movePointer:: Int -> Int-> IO()
movePointer width height = setCursorPosition height width

menu :: Int -> Int ->IO()
menu width height = do
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    displayRetangulo (retangulo width height) width height
    movePointer ((width `div` 2) -4) 3
    setSGR [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink,SetColor Foreground Vivid Red]
    putStr("BOMBERMAN")
    setSGR [SetConsoleIntensity NormalIntensity, SetBlinkSpeed NoBlink, SetColor Foreground Vivid Red]
    movePointer ((width `div` 2) - 4) 4
    putStr("[1] JOGAR")
    movePointer ((width `div` 2) -4) 5
    putStr("[2] SAIR")
    setSGR [SetConsoleIntensity BoldIntensity, SetBlinkSpeed NoBlink, SetColor Foreground Vivid Red]