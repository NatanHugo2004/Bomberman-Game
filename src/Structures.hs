module Structures where

newtype Point = Point (Int, Int) deriving (Eq)

data Map = Map { 
        walls :: [Point],
        player :: Point
    }

data GameConfigs = GameConfigs {
        height :: Int,
        width :: Int
    }

createPoint :: Int -> Int -> Point
createPoint x y = (Point (x, y))

takeX :: Point -> Int
takeX (Point(x, y)) = x

takeY :: Point -> Int
takeY (Point(x, y)) = y
