-- | Módulo principal do jogo Bomberman, responsável por iniciar o jogo e gerenciar o menu.
module Main (main) where

import System.IO
import System.Console.ANSI
import Structures
import GameLoop (startGame)
import Menu

-- | Função principal que configura o terminal, exibe o menu inicial e inicia o jogo conforme escolha do usuário.
main :: IO ()
main = do
    hideCursor
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let gameConfigs = GameConfigs 8 18 120 
    menu (width gameConfigs) ((height gameConfigs) + 1)
    hFlush stdout
    escolha <- getInput
    setSGR [Reset]
    if escolha == '1' then do
        instructions (width gameConfigs) (height gameConfigs)
        hFlush stdout
        input <- getEnter
        if input == '\n' then
            startGame gameConfigs
        else
            return()
    else do
        menuExit ((height gameConfigs) + 1)
        showCursor


    
