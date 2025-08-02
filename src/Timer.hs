module Timer where

import Control.Concurrent
import Data.IORef
import Control.Monad
import Map
import Structures
import Data.List

startTimer :: IORef Int -> IO ()
startTimer tempoRef = do
    _ <- forkIO $ countdown tempoRef 
    return ()

countdown :: IORef Int -> IO ()
countdown tempoRef = do
    let loop = do
            tempo <- readIORef tempoRef
            when (tempo >= 0) $ do
                threadDelay 1000000
                writeIORef tempoRef (tempo - 1)
                loop
    loop

updateBombTimers :: IORef Map -> IO ()
updateBombTimers mapRef = do
    let loop = do
            threadDelay 1000000
            mapa <- readIORef mapRef
            let bombasAtualizadas = map updateBomb (bombs mapa)
            let (explodidas, ativas) = partition (\b -> timer b <= 0) bombasAtualizadas
            let mapaExplodido = explodeBombs mapa explodidas
            let novoMapa = mapaExplodido { bombs = ativas }
            writeIORef mapRef novoMapa
            loop
    loop
