module Menu where
import System.Console.ANSI
import System.IO (stdout)
import Walls
retangulo = [(x, y) | x <- [0..width], y <- [0, height]] ++ [(x, y) | x <- [0,width], y <- [1..height - 1]]
verificaCaractere :: (Int,Int) -> String
verificaCaractere (n,m)
 | n == 0 && m == 0 || n == 0 && m == height || n == width && m == 0 || n == width && m == height  = "+"
 | n == 0 || n == width   = "|"
 | m == 0 || m == height  = "-"



displayRetangulo :: [(Int, Int)] -> IO()
displayRetangulo []  = return ()
displayRetangulo (h:hs) = do
                        movePointer (takeX h) (takeY h) 
                        putStr(verificaCaractere h)
                        displayRetangulo hs
menu :: IO()
menu = do
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    displayRetangulo retangulo
    movePointer ((width `div` 2) -4) 3
    setSGR [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink,SetColor Foreground Vivid Red]
    putStr("BOMBERMAN")
    setSGR [SetConsoleIntensity NormalIntensity, SetBlinkSpeed NoBlink, SetColor Foreground Vivid Red]
    movePointer ((width `div` 2) - 4) 4
    putStr("[1] JOGAR")
    movePointer ((width `div` 2) -4) 5
    putStr("[2] SAIR")
    setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Red]
