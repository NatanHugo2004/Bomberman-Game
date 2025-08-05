module Utils where

import Structures

createPoint :: Int -> Int -> Point
createPoint x y = (Point (x, y))

takeX :: Point -> Int
takeX (Point(x, y)) = x

takeY :: Point -> Int
takeY (Point(x, y)) = y

neighbors :: Point -> [Point]
neighbors (Point (x, y)) = [ createPoint x     y, 
                             createPoint (x+1) y,
                             createPoint (x-1) y,
                             createPoint x     (y+1),
                             createPoint x     (y-1) ]

allBombsPoints :: [Bomb] -> [Point]
allBombsPoints bombs = [(bombPosition b) | b <- bombs] 

allExplosionsPoints :: [Explosion] -> [Point]
allExplosionsPoints explosions = [e | explosion <- explosions, 
                                      e         <- (explosionPosition explosion)] 
