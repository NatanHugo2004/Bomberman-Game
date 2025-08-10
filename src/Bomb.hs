--Modulo responsável por todas as operações relacionadas a estrutura bomba e explosões, estruturas essas definidas no modulo structure.
module Bomb where

import Structures
import Utils


--Função responsável por plantar bombas em uma determinada posição
-- | @param position Point: o ponto a qual deseja-se plantar a bomba
-- | @param timer Int: o tempo que a bomba vai demorar para explodir
-- | return Bomb: uma bomba com a posição e o tempo fornecido
plantBomb:: Point -> Int -> Bomb
plantBomb position timer = Bomb { bombPosition = position,
                                  timer        = timer }

--Função que adiciona uma bomba em uma lista de bombas
-- | @param newBomb Bomb: uma nova bomba para ser adicionada na lista
-- | @param bombs [Bomb]: uma lista de bombas, a qual será adicionada a nova bomba
-- | @return [Bomb]: uma lista de bombas, que contém a nova bomba passada por parâmetro, adicionando ela apenas se houverem menos de 3 bombass na lista.
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
    raioBomba = [createPoint (takeX pos + dx) (takeY pos + dy) | dx <- [-raio..raio],
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

--Função responsável por retornar todos os pontos de explosão de uma lista de explosões.
--[
getExplosionsPoints :: [Explosion] -> [Point] -> [Point]
getExplosionsPoints [] points = points
getExplosionsPoints (j:js) points = getExplosionsPoints js (points ++ (explosionPosition j))

--Função responsável por verificar se um player está morto ou não
-- | @param playerPosition Point: um ponto representando a posição do player
-- | @param explosions [Explosion]: uma lista de explosões
-- | @return Bool: um valor booleano, sendo verdadeiro caso a posição fornecida esteja em algum ponto atingido por explosões 
isDead :: Point -> [Explosion] -> Bool
isDead playerPosition explosions = playerPosition `elem` (getExplosionsPoints explosions []) 

