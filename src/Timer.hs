-- | Módulo responsável por gerenciar os temporizadores do jogo, incluindo o tempo do jogo e as bombas.
module Timer (startTimer, startBombTimers, stopThreads) where

import Control.Concurrent
import Data.IORef
import Control.Monad
import Data.List (partition)
import Bomb
import Structures

-- | Função responsável por iniciar o temporizador do jogo.
-- | @param tempoRef IORef Int: referência para o tempo restante do jogo
-- | @return ThreadId: identificador da thread que está executando o temporizador
startTimer :: IORef Int -> IO ThreadId
startTimer tempoRef = forkIO $ countdown tempoRef

-- | Função responsável por realizar a contagem regressiva do tempo do jogo.
-- | @param tempoRef IORef Int: referência para o tempo restante do jogo
countdown :: IORef Int -> IO ()
countdown tempoRef = do
    let loop = do
            tempo <- readIORef tempoRef
            when (tempo >= 0) $ do
                threadDelay 1000000
                writeIORef tempoRef (tempo - 1)
                loop
    loop

-- | Função responsável por iniciar os temporizadores das bombas.
-- | @param mapRef IORef Map: referência para o mapa do jogo, que contém as bombas
-- | @return ThreadId: identificador da thread que está executando os temporizadores das bombas
startBombTimers :: IORef Map -> IO ThreadId
startBombTimers mapRef = forkIO $ updateBombTimers mapRef

-- | Função responsável por atualizar os temporizadores das bombas e explosões no mapa.
-- | @param mapRef IORef Map: referência para o mapa do jogo, que contém as bombas
updateBombTimers :: IORef Map -> IO ()
updateBombTimers mapRef = do
    let loop = do
            threadDelay 1000000
            mapa <- readIORef mapRef
            let bombasAtualizadas = map updateBomb (bombs mapa)
            let explosionAtualizadas = map updateExplosion (explosions mapa)
            let mapaComTimers = mapa { bombs = bombasAtualizadas, explosions = explosionAtualizadas }
            let (explodidas, ativas) = partition (\b -> timer b <= 0) (bombs mapaComTimers)
            let mapaExplodido = explodeBombs mapaComTimers explodidas
            let explosionAtivas = filter (\e -> time e > 0) (explosions mapaExplodido)
            let novoMapa = mapaExplodido { bombs = ativas, explosions = explosionAtivas }
            writeIORef mapRef novoMapa
            loop
    loop

-- | Função responsável por parar todas as threads de temporização.
-- | @param tids [ThreadId]: lista de identificadores de threads a serem paradas
stopThreads :: [ThreadId] -> IO ()
stopThreads tids = mapM_ killThread tids