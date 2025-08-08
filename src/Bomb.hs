--Modulo responsável por todas as operações relacionadas a estrutura bomba e explosões, estruturas essas definidas no modulo structure.
module Bomb where

import Structures
import Utils


--Função responsável por plantar bombas em uma determinada posição. Recebe um ponto e um inteiro, representando o local que a bomba será plantada, e o tempo de explosão e retornando uma bomba.
plantBomb:: Point -> Int -> Bomb
plantBomb position timer = Bomb {bombPosition = position,
                                 timer = timer
                                }

addBomb :: Bomb -> [Bomb] -> [Bomb]
addBomb newBomb bombs
    | length bombs < 3 = newBomb : bombs  
    | otherwise        = bombs

updateBomb:: Bomb -> Bomb
updateBomb bomb = Bomb (bombPosition bomb) ((timer bomb) -1)

explodeBombs :: Map -> [Bomb] -> Map
explodeBombs mapa [] = mapa
explodeBombs mapa (b:bs) = explodeBombs mapaAtualizado bs
  where
    pos = bombPosition b
    raio = 1
    raioBomba =
        [createPoint (takeX pos + dx) (takeY pos + dy) |
            dx <- [-raio..raio],
            dy <- [-raio..raio],
            abs dx + abs dy <= raio]
    pontosAfetados = filter(`notElem` walls mapa) raioBomba
    explosion = createExplosion (pontosAfetados)
    boxesRestantes = filter (`notElem` pontosAfetados) (boxes mapa)
    mapaAtualizado = mapa { boxes = boxesRestantes, explosions = explosion: explosions mapa}

createExplosion :: [Point] -> Explosion
createExplosion points = Explosion (points) (1)

updateExplosion :: Explosion -> Explosion
updateExplosion explosion = Explosion (explosionPosition explosion) ((time explosion) -1)

getExplosionsPoints :: [Explosion] -> [Point] -> [Point]
getExplosionsPoints [] points = points
getExplosionsPoints (j:js) points = getExplosionsPoints js (points ++ (explosionPosition j))

isDead :: Point -> [Explosion] -> Bool
isDead playerPosition explosions = playerPosition `elem` (getExplosionsPoints explosions []) 

