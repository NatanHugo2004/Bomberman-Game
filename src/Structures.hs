module Structures where

newtype Point = Point (Int, Int) deriving (Eq)

data Map = Map { walls      :: [Point],
                 boxes      :: [Point],
                 player     :: Point,
                 bombs      :: [Bomb],
                 explosions :: [Explosion],
                 door       :: Point,
                 key        :: Maybe Point,
                 hasKey     :: Bool } 

data GameConfigs = GameConfigs { height     :: Int,
                                 width      :: Int,
                                timerGamer :: Int }

data Explosion = Explosion { explosionPosition :: [Point],
                             time              :: Int }

data Bomb = Bomb { bombPosition :: Point,
                   timer        :: Int }
