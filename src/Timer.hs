module Timer (startTimer, countdown) where

import Control.Concurrent
import Data.IORef
import Control.Monad

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
