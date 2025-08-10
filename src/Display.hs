module Display where

import Structures
import Utils
import System.Console.ANSI
import System.IO

data Symboll = S_wall | S_box | S_player | S_bomb | S_explosion | S_playerDeath | S_key | S_door

symbollToChar :: Symboll -> Char
symbollToChar S_wall        = '█'
symbollToChar S_box         = '▓'
symbollToChar S_player      = '𖦔'
symbollToChar S_bomb        = 'δ'
symbollToChar S_explosion   = '𖤌'
symbollToChar S_playerDeath = '𖣛'
symbollToChar S_key         = '𖠞'
symbollToChar S_door        = '⇭'

movePointer :: Int -> Int -> IO()
movePointer x y = setCursorPosition y x

displayPoint :: Point -> Symboll -> [SGR] -> IO ()
displayPoint point symboll sgr = do 
    movePointer (takeX point) (takeY point)
    setSGR sgr
    putChar (symbollToChar symboll)
    setSGR [Reset]

displayPoints :: [Point] -> Symboll -> [SGR] -> IO ()
displayPoints [] _ _ = return ()
displayPoints (p:ps) symboll sgr = do
    displayPoint p symboll sgr
    displayPoints ps symboll sgr

displayTimer :: GameConfigs -> Int -> IO ()
displayTimer configs time = do
    movePointer 0 ((height configs) + 2)
    clearLine
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "══ Remaining Time ══\n"
    setSGR [Reset]
    putStr $ "│" ++ filled ++ empty ++ "│ "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr $ show time ++ "s "
    setSGR [Reset]
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "\n════════════════════\n"
    setSGR [Reset]
    hFlush stdout
    where
        total  = timerGamer configs
        filled = replicate (time * 13 `div` total) '■'
        empty  = replicate (13 - length filled) ' '
        

display :: Map -> GameConfigs -> Int -> IO()
display map configs time = do
    clearScreen
    displayPoints (walls map) S_wall [Reset]
    case key map of
        Just k -> displayPoint k S_key sgrKey
        Nothing -> return ()
    displayPoints (boxes map) S_box sgrBox
    displayPoints (allBombsPoints (bombs map)) S_bomb sgrBomb 
    displayPoints (allExplosionsPoints (explosions map)) S_explosion sgrExplosion
    displayPoint (door map) S_door sgrDoor 
    displayPoint  (player map) S_player [Reset] 
    displayTimer configs time
    hFlush stdout
    where
        sgrBox = [SetColor Foreground Dull Yellow]
        sgrBomb = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid White]
        sgrExplosion = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
        sgrKey = [SetColor Foreground Vivid Yellow]
        sgrDoor = [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Cyan]

