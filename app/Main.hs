module Main (main) where

import Structures
import GameLoop
import Map 
import Menu
import System.IO 

main :: IO ()
main = do
    --configura o terminal para ler a entrada sem buffer
    -- e para não exibir os caracteres digitados
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    --Define as configurações do jogo e cria o mapa inicial
    let gameConfigs = GameConfigs { height = 8, width = 18}
    let initialMap = createMap (height gameConfigs) (width gameConfigs)

    -- Inicia o loop principal do jogo
    gameLoop initialMap

    














    --startGame

--startGame :: IO ()
--startGame = do
  --  let gameConfigs = GameConfigs 8 18
    --let initialMap = createMap (height gameConfigs) (width gameConfigs)
    --gameLoop initialMap