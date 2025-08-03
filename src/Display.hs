module Display where

import Structures
import System.Console.ANSI
import System.IO

data Symboll = S_wall | S_box | S_player | S_bomb | S_explosion | S_playerDeath

symbollToChar :: Symboll -> Char
symbollToChar S_wall        = '█'
symbollToChar S_box         = '𖧭'
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

displayPlayerDeath :: Point -> IO()
displayPlayerDeath player = do 
    movePointer (takeX player) (takeY player)
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
    putChar (symbollToChar S_playerDeath)
    setSGR [Reset]

displayBombs :: [Bomb] -> IO()
displayBombs [] = do
    setSGR [Reset]
    return ()
displayBombs (e:es) = do
    displayPoint (bombPosition e) S_bomb sgrBomb
    displayBombs es
    where
        sgrBomb = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid White]

displayExplosions :: [Explosion] -> IO()
displayExplosions [] = return ()
displayExplosions (e:es) = do
    displayPoints (explosionPosition e) S_explosion sgrExplosion  
    displayExplosions es
    where
        sgrExplosion = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

display :: Map -> IO()
display map = do
    clearScreen
    displayPoints (walls map) S_wall [Reset]
    displayPoints (boxes map) S_box sgrBox
    displayBombs (bombs map)
    displayExplosions (explosions map)
    displayPoint (player map) S_player [Reset] 
    hFlush stdout
    where
        sgrBox = [SetColor Foreground Dull Yellow]


