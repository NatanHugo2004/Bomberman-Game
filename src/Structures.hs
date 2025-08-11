-- | Módulo responsável por definir as estruturas de dados do jogo.
module Structures where

-- | Definição de um ponto no mapa, representado por coordenadas X e Y.
newtype Point = Point (Int, Int) deriving (Eq)

-- | Estrutura que representa o mapa do jogo, contendo paredes, caixas, player, bombas, explosões, porta e chave.
data Map = Map { walls      :: [Point],
                 boxes      :: [Point],
                 player     :: Point,
                 bombs      :: [Bomb],
                 explosions :: [Explosion],
                 door       :: Point,
                 key        :: Maybe Point,
                 hasKey     :: Bool } 

-- | Estrutura que define as configurações do jogo, como altura, largura e tempo total.
data GameConfigs = GameConfigs { height     :: Int,
                                 width      :: Int,
                                timerGamer :: Int }

-- | Estrutura que representa uma explosão no mapa, contendo a posição e o tempo restante da explosão.
data Explosion = Explosion { explosionPosition :: [Point],
                             time              :: Int }

-- | Estrutura que representa uma bomba no mapa, contendo a posição da bomba e o tempo restante para explodir.
data Bomb = Bomb { bombPosition :: Point,
                   timer        :: Int }
