module Structures where

newtype Point = Point (Int, Int) deriving (Eq)

data Explosion = Explosion {
     explosionPosition :: [Point],
     time :: Int}

data Bomb = Bomb {
    bombPosition :: Point,
    timer :: Int 
}

data Map = Map { 
        walls :: [Point],
        boxes :: [Point],
        player :: Point,
        bombs :: [Bomb],
        explosions :: [Explosion]
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
