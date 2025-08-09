--Modulo responsável por todas as operações relacionadas a estrutura bomba e explosões, estruturas essas definidas no modulo structure.
module Bomb where

import Structures
import Utils


--Função responsável por plantar bombas em uma determinada posição. Recebe um ponto e um inteiro, representando o local que a bomba será plantada, e o tempo de explosão e retornando uma bomba.
plantBomb:: Point -> Int -> Bomb
plantBomb position timer = Bomb {bombPosition = position,
                                 timer = timer
                                }

--Função que adiciona uma bomba em uma lista de bombas, recebendo uma bomba, uma lista de bombas, e retornando uma nova lista de bombas, feita a partir da lista de bombas recebidas + a bomba passada como parâmetro. A bomba só será adicionada caso a lista tenha menos que 3 bombas.
addBomb :: Bomb -> [Bomb] -> [Bomb]
addBomb newBomb bombs
    | length bombs < 3 = newBomb : bombs  
    | otherwise        = bombs

-- Função responsável por atualizar a bomba, diminuindo seu tempo, recebendo uma bomba e retornando uma nova bomba com o campo timer decrementado.
updateBomb:: Bomb -> Bomb
updateBomb bomb = Bomb (bombPosition bomb) ((timer bomb) -1)

-- Função responsável por explodir bombas de um mapa, recebendo um mapa, uma lista de bombaas, retornaando um novo mapa com as bombas explodidas e uma nova explosão criada.
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

--Função responsável por criar uma nova explosão. Recebendo uma lista de pontos e retornadno uma explosão.
createExplosion :: [Point] -> Explosion
createExplosion points = Explosion (points) (1)

--Função responsável por atualizar uma explosão, recebendo uma explosão e retornando uma nova explosão, com os mesmos campos da anterior, porém com o timer decrementado.
updateExplosion :: Explosion -> Explosion
updateExplosion explosion = Explosion (explosionPosition explosion) ((time explosion) -1)

--Função responsável por retornar todos os pontos de explosão de uma lista de explosões. Recebe uma lista de explosões, uma lista de pontos e retorna uma lista de pontos, que representa os pontos afetados por explosões.
getExplosionsPoints :: [Explosion] -> [Point] -> [Point]
getExplosionsPoints [] points = points
getExplosionsPoints (j:js) points = getExplosionsPoints js (points ++ (explosionPosition j))

--Função responsável por verificar se um player está morto ou não, para isso, ele recebe um ponto do mapa(o ponto do player), uma lista de explosões, e verifica se o player está em algum ponto de explosão. 
isDead :: Point -> [Explosion] -> Bool
isDead playerPosition explosions = playerPosition `elem` (getExplosionsPoints explosions []) 