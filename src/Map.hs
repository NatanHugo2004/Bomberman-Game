-- Este módulo tem como objetivo administrar operações pertinentes a estrutura de mapa, definida no módulo mapa. Bem como todas as operações referentes aos campos de um mapa atual.
module Map where

import Structures
import Utils
import Bomb
import System.Random.Shuffle
import System.Random (StdGen) 

--Função responsável captar a direção que o player está indo.
-- | @param c Char: um caractere que representa a direção a qual o player quer ir.
-- | @return (Point -> Point): uma função que recebe um ponto e retorna um ponto, sendo esta função definida dentro de charToDirection como move.
charToDirection :: Char -> (Point -> Point)
charToDirection c = case c of
    'a' -> move (-1) 0
    'd' -> move 1    0
    'w' -> move 0    (-1)
    's' -> move 0    1
    _   -> id
    where
        move x y = \p -> createPoint ((takeX p) + x) ((takeY p) + y)

--Função responsável por verificar a validade da posição do player. 
-- | @param map Map: um mapa para ser analisado
-- | @param newPosition Point: um ponto no mapa, para conferir se é uma posição válida para o player
-- | @return Bool: um booleano representando se a posição é válida ou não, a posição será válida se nela não houver paredes, caixas ou bombas.
isValidPlayerPos :: Map -> Point -> Bool
isValidPlayerPos map newPosition = not ((isWall newPosition (walls map)) || 
                                        (isBox  newPosition (boxes map)) || 
                                        (isBomb newPosition (bombs map)))

--Função responsável por criar paredes.
-- | @param height Int: a altura do mapa, para criar paredes condizentes com o mapa
-- | @param width Int: a largura do mapa, para criar paredes condizentes com o mapa
-- | @return [Point]: uma lista de pontos representando as paredes
createWalls :: Int -> Int -> [Point]
createWalls height width = [createPoint x y | x <- [0..width], y <- [0, height]] ++ 
                           [createPoint x y | x <- [0,width], y <- [1..height - 1]] ++ 
                           [createPoint x y | x <- [2, 4..width - 2], y <- [2, 4..height - 2]]

--Função responsável por criar caixas destrutivas
-- | @param height Int: a altura do mapa, para as caixas serem condizentes com o tamanho do mapa
-- | @param width Int: largura do mapa, para as caixas serem condizentes com o tamanho do mapa
-- | @param walls [Point]: uma lista de ponto de paredes, para não serem geradas caixas onde existem paredes
-- | @param player Point: um player, para saber a posição do player de forma que o jogo seja vencível
-- | @param door Point: um ponto que representa a porta, para não serem geradas caixas onde existe a porta
-- | @param gen StdGen: um conjunto de números aleatórios para gerar aleatóriedade nas caixas
-- | @return [Point]: uma lista de pontos representando as caixas destrutivas criadas
createBoxes :: Int -> Int -> [Point] -> Point -> Point -> StdGen -> [Point]
createBoxes height width walls player door gen = take boxesAmount shuffled
    where
        allPoints    = [createPoint x y | x <- [1..width-1], y <- [1..height-1]]
        isValidPoint = \p -> not (p `elem` walls || p `elem` (neighbors player) || p == door)
        validPoints  = filter isValidPoint allPoints
        shuffled     = shuffle' validPoints (length validPoints) gen
        boxesAmount  = ceiling (0.70 * fromIntegral (length validPoints) :: Double)

--Função responsável por criar um mapa
-- | @param height Int: altura do mapa
-- | @param width Int: largura do mapa
-- | @param gen StdGen: um conjunto de números aleatórios, para configurar as caixas destrutivas
-- | @return Map: um mapa inicializado pronto para começar o jogo
createMap :: Int -> Int -> StdGen -> Map
createMap height width gen = Map walls boxes player [] [] door (Just keyPosition)  False
  where
    walls       = createWalls height width
    player      = createPoint 1 1
    door        = createPoint (width - 1) (height - 1) 
    boxes       = createBoxes height width walls player door gen
    keyPosition = createKeyPosition boxes player gen

--Função responsável por atualizar um mapa, como na programação funcional os estados não são alterados.
-- | @param map Map: um mapa que será atualizado 
-- | @param input Char: um caractere, responsável por sinalizar a ação do player
-- | @return Map: um novo mapa
updateMap :: Map -> Char -> Map
updateMap map input
 | input == ' '                      = map {bombs = addBomb newBomb (bombs map)}
 | otherwise    =
    let
      playerPos    = player map
      direction    = charToDirection input
      newPlayerPos = direction playerPos
    in
      if isValidPlayerPos map newPlayerPos
        then collectKey map newPlayerPos
        else map
  where
    newBomb = plantBomb (player map) 3

--Função responsável por criar uma posição para a chave, garantindo que não esteja em um bloco vizinho ao jogador.
-- | @param boxes [Point]: lista de posições de caixas
-- | @param player Point: posição atual do jogador
-- | @param gen StdGen: gerador de números aleatórios
-- | @return Point: posição gerada para a chave
createKeyPosition ::[Point] -> Point -> StdGen -> Point
createKeyPosition boxes player gen = head $ shuffle' validBoxes (length validBoxes) gen
  where
    isValidBox p = not (p `elem` (neighbors player))
    validBoxes  = filter isValidBox boxes   

--Função responsável por atualizar o estado do mapa quando o jogador coleta a chave.
-- | @param map Map: o mapa atual
-- | @param newPlayerPos Point: a nova posição do jogador
-- | @return Map: o mapa atualizado, com a chave coletada se o jogador estiver na posição da chave
collectKey :: Map -> Point -> Map
collectKey mapa newPlayerPos = 
  case key mapa of
    Just k -> 
      if newPlayerPos == k
        then mapa { player = newPlayerPos, hasKey = True, key = Nothing }
        else mapa { player = newPlayerPos }
    Nothing -> mapa { player = newPlayerPos }  

--Função responsável por verificar se o jogador pode sair pela porta.
-- | @param mapa Map: o mapa atual
-- | @param newPlayerPos Point: a nova posição do jogador
-- | @return Bool: um booleano representando se o jogador pode sair pela porta, ou seja, se o jogador está na posição da porta e já coletou a chave
canExitThroughDoor :: Map -> Point -> Bool
canExitThroughDoor mapa newPlayerPos =
  newPlayerPos == door mapa && hasKey mapa

--Função responsável por verificar se um elemento está em uma lista.
-- | @param a Eq a => a: elemento a ser verificado
-- | @param [a]: lista onde será feita a verificação
-- | @return Bool: True se o elemento está na lista, False caso contrário
isIn :: Eq a => a -> [a] -> Bool
isIn = elem

--Função responsável por verificar se uma posição é uma parede.
-- | @param Point: ponto a ser verificado
-- | @param [Point]: lista de posições de paredes
-- | @return Bool: True se a posição é uma parede
isWall :: Point -> [Point] -> Bool
isWall = isIn

--Função responsável por verificar se uma posição é uma caixa.
-- | @param Point: ponto a ser verificado
-- | @param [Point]: lista de posições de caixas
-- | @return Bool: True se a posição é uma caixa
isBox :: Point -> [Point] -> Bool
isBox = isIn

--Função responsável por verificar se uma posição contém uma bomba.
-- | @param point Point: posição a ser verificada
-- | @param bombs [Bomb]: lista de bombas
-- | @return Bool: True se a posição contém uma bomba
isBomb:: Point -> [Bomb] -> Bool
isBomb point bombs = isIn point (map bombPosition bombs)
