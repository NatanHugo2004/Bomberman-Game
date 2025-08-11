-- | Módulo responsável pelo loop principal do jogo.
module GameLoop where

import Structures
import System.IO
import Map 
import Display
import Menu
import Data.IORef
import Bomb
import System.Console.ANSI
import Control.Concurrent
import System.Random
import Timer

-- | Função responsável pelo loop principal do jogo, atualizando o estado, tratando entradas e verificando condições de vitória ou derrota.
-- | @param mapRef IORef Map: referência mutável para o estado atual do mapa
-- | @param configs GameConfigs: configurações do jogo
-- | @param tempoRef IORef Int: referência mutável para o tempo restante
-- | @param bombTID ThreadId: identificador da thread responsável pelas bombas
-- | @param timerID ThreadId: identificador da thread responsável pelo timer
-- | @return IO(): efeito colateral que mantém o jogo em execução até vitória, derrota ou saída
gameLoop :: IORef Map -> GameConfigs -> IORef Int -> ThreadId -> ThreadId -> IO ()
gameLoop mapRef configs tempoRef bombTID timerID = do
    map <- readIORef mapRef
    time <- readIORef tempoRef
    display map configs time
    threadDelay 1000 
    if (checkGameOver map time) then do 
        displayPoint (player map) S_playerDeath sgrPlayerDeath
        hFlush stdout
        gameOverScreen configs
        playAgainScreen configs
        input <- getInput
        if input == '1' then do
            stopThreads [bombTID, timerID]
            startGame configs
        else 
            byeScreen configs
    else do   
        inputAvailable <- hReady stdin 
        if inputAvailable then do
            input <- getChar
            if input == 'q' then 
                quitScreen configs
            else do
                let newMap = updateMap map input
                if canExitThroughDoor newMap (player newMap) then do
                    gameWinScreen configs
                    playAgainScreen configs
                    playInput <- getInput
                    if playInput == '1' then do
                        stopThreads [bombTID, timerID]
                        startGame configs
                    else do
                        byeScreen configs
                else do
                    writeIORef mapRef newMap
                    gameLoop mapRef configs tempoRef bombTID timerID
        else do
            gameLoop mapRef configs tempoRef bombTID timerID
    where
        sgrPlayerDeath = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]

-- | Função responsável por verificar se o jogador perdeu o jogo, seja por tempo esgotado ou por estar em uma posição de explosão.
-- | @param map Map: o estado atual do mapa
-- | @param time Int: o tempo restante do jogo
-- | @return Bool: um booleano representando se o jogador perdeu o jogo
checkGameOver :: Map -> Int -> Bool
checkGameOver map time = time <= 0 || isDead (player map) (explosions map)

-- | Função responsável por iniciar o jogo, configurando o ambiente e iniciando o loop principal.
-- | @param gameConfigs GameConfigs: configurações do jogo, incluindo altura e largura do mapa
startGame :: GameConfigs -> IO ()
startGame gameConfigs = do
    hideCursor
    tempoRef <- newIORef (timerGamer gameConfigs) 
    timerID <- startTimer tempoRef
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    gen <- newStdGen
    initialMap <- newIORef (createMap (height gameConfigs) (width gameConfigs) gen)
    bombTID <- startBombTimers initialMap
    gameLoop initialMap gameConfigs tempoRef bombTID timerID
