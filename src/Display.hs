module Display where

import Structures
import Utils
import System.Console.ANSI
import System.IO

data Symboll = S_wall | S_box | S_player | S_bomb | S_explosion | S_playerDeath

symbollToChar :: Symboll -> Char
symbollToChar S_wall        = '█'
symbollToChar S_box         = '▓'
symbollToChar S_player      = '𖦔'
symbollToChar S_bomb        = 'δ'
symbollToChar S_explosion   = '𖤌'
symbollToChar S_playerDeath = '𖣛'

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

display :: Map -> IO()
display map = do
    clearScreen
    displayPoints (walls map) S_wall [Reset]
    displayPoints (boxes map) S_box sgrBox
    displayPoints (allBombsPoints (bombs map)) S_bomb sgrBomb 
    displayPoints (allExplosionsPoints (explosions map)) S_explosion sgrExplosion
    displayPoint  (player map) S_player [Reset] 
    hFlush stdout
    where
        sgrBox = [SetColor Foreground Dull Yellow]
        sgrBomb = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid White]
        sgrExplosion = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]


