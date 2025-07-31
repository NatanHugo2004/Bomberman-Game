module Map where

import Structures 

-- TODO: VERIFICAR SE DÁ PRA FAZER ALGO MAIS BONITO
charToDirection :: Char -> (Point -> Point)
charToDirection 'a' = \p -> createPoint ((takeX p) - 1) (takeY p)
charToDirection 'd' = \p -> createPoint ((takeX p) + 1) (takeY p)
charToDirection 'w' = \p -> createPoint (takeX p) ((takeY p) - 1)
charToDirection 's' = \p -> createPoint (takeX p) ((takeY p) + 1)
charToDirection c = \p -> createPoint (takeX p) (takeY p)

movePlayer :: (Point -> Point) -> Point -> Point
movePlayer direction player = direction player

isValidPlayerPosition :: Map -> Point -> Bool
isValidPlayerPosition map newPosition = not (elem newPosition (walls map))

createMap :: Int -> Int -> Point -> Map
createMap height width point = Map walls point
    where
        walls = [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

updateMap :: Map -> Char -> Map
updateMap map input = 
    Map (walls map) 
        (if (isValidPlayerPosition map newPlayerPosition) then
            newPlayerPosition 
        else 
            (player map)) 
    where 
        direction = charToDirection input
        newPlayerPosition = movePlayer direction (player map)
