-- | Módulo responsável por fornecer utilitários e funções auxiliares para o jogo.
module Utils where

import Structures

-- | Função responsável por criar um ponto a partir de coordenadas X e Y.
-- | @param x Int: coordenada X do ponto
-- | @param y Int: coordenada Y do ponto
-- | @return Point: um ponto representado pelas coordenadas fornecidas
createPoint :: Int -> Int -> Point
createPoint x y = (Point (x, y))

-- | Função responsável por extrair a coordenada X de um ponto.
-- | @param Point: um ponto do qual se deseja extrair a coordenada X
-- | @return Int: a coordenada X do ponto
takeX :: Point -> Int
takeX (Point(x, y)) = x

-- | Função responsável por extrair a coordenada Y de um ponto.
-- | @param Point: um ponto do qual se deseja extrair a coordenada Y
-- | @return Int: a coordenada Y do ponto
takeY :: Point -> Int
takeY (Point(x, y)) = y

-- | Função responsável por obter todos os vizinhos de um ponto.
-- | @param Point: um ponto do qual se deseja obter os vizinhos
-- | @return [Point]: uma lista de pontos representando os vizinhos do ponto fornecido incluindo o próprio ponto
neighbors :: Point -> [Point]
neighbors (Point (x, y)) = [ createPoint x     y, 
                             createPoint (x+1) y,
                             createPoint (x-1) y,
                             createPoint x     (y+1),
                             createPoint x     (y-1) ]

-- | Função responsável por obter a lista de posições de todas as bombas no mapa.
-- | @param bombs [Bomb]: lista de bombas
-- | @return [Point]: lista de posições das bombas
allBombsPoints :: [Bomb] -> [Point]
allBombsPoints bombs = [(bombPosition b) | b <- bombs] 

-- | Função responsável por obter a lista de posições de todas as explosões no mapa.
-- | @param explosions [Explosion]: lista de explosões
-- | @return [Point]: lista de posições das explosões
allExplosionsPoints :: [Explosion] -> [Point]
allExplosionsPoints explosions = [e | explosion <- explosions, 
                                      e         <- (explosionPosition explosion)] 
