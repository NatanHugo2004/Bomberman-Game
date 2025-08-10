-- Módulo responsável por exibir o mapa e os elementos do jogo na tela.
module Display where

import Structures
import Utils
import System.Console.ANSI
import System.IO

-- Definição dos símbolos gráficos usados no jogo.
data Symboll = S_wall | S_box | S_player | S_bomb | S_explosion | S_playerDeath | S_key | S_door

-- Função responsável por converter um Symboll no caractere gráfico correspondente.
-- | @param Symboll: um símbolo a ser convertido
-- | @return Char: caractere que representa o símbolo na tela
symbollToChar :: Symboll -> Char
symbollToChar S_wall        = '█'
symbollToChar S_box         = '▓'
symbollToChar S_player      = '𖦔'
symbollToChar S_bomb        = 'δ'
symbollToChar S_explosion   = '𖤌'
symbollToChar S_playerDeath = '𖣛'
symbollToChar S_key         = '⚷'
symbollToChar S_door        = 'በ'

-- Função responsável por mover o cursor para uma posição específica no terminal.
-- | @param x Int: coordenada X (coluna)
-- | @param y Int: coordenada Y (linha)
movePointer :: Int -> Int -> IO()
movePointer x y = setCursorPosition y x

-- Função responsável por exibir um único ponto no terminal.
-- | @param point Point: posição do ponto no mapa
-- | @param symboll Symboll: símbolo gráfico que será exibido
-- | @param sgr [SGR]: lista de configurações de estilo ANSI para o símbolo
displayPoint :: Point -> Symboll -> [SGR] -> IO ()
displayPoint point symboll sgr = do 
    movePointer (takeX point) (takeY point)
    setSGR sgr
    putChar (symbollToChar symboll)
    setSGR [Reset]

-- Função responsável por exibir múltiplos pontos no terminal.
-- | @param [Point]: lista de posições
-- | @param symboll Symboll: símbolo a ser usado em todas as posições
-- | @param sgr [SGR]: estilo ANSI aplicado a todos os pontos
displayPoints :: [Point] -> Symboll -> [SGR] -> IO ()
displayPoints [] _ _ = return ()
displayPoints (p:ps) symboll sgr = do
    displayPoint p symboll sgr
    displayPoints ps symboll sgr

-- Função responsável por exibir o temporizador do jogo no terminal, mostrando uma barra de progresso preenchida de acordo com o tempo restante.
-- | @param configs GameConfigs: configurações do jogo (para altura do mapa e tempo total)
-- | @param time Int: tempo restante em segundos
displayTimer :: GameConfigs -> Int -> IO ()
displayTimer configs time = do
    movePointer 0 ((height configs) + 2)
    clearLine
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "══ Remaining Time ══\n"
    setSGR [Reset]
    putStr $ "│" ++ filled ++ empty ++ "│ "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr $ show time ++ "s "
    setSGR [Reset]
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    putStr "\n════════════════════\n"
    setSGR [Reset]
    hFlush stdout
    where
        total  = timerGamer configs
        filled = replicate (time * 13 `div` total) '■'
        empty  = replicate (13 - length filled) ' '
        
-- Função responsável por exibir todos os elementos do jogo no terminal(paredes, caixas, bombas, explosões, chave, porta, jogador, temporizador).
-- | @param map Map: estado atual do mapa
-- | @param configs GameConfigs: configurações do jogo
-- | @param time Int: tempo restante em segundos
display :: Map -> GameConfigs -> Int -> IO()
display map configs time = do
    clearScreen
    displayPoints (walls map) S_wall [Reset]
    case key map of
        Just k -> displayPoint k S_key sgrKey
        Nothing -> return ()
    displayPoints (boxes map) S_box sgrBox
    displayPoints (allBombsPoints (bombs map)) S_bomb sgrBomb 
    displayPoints (allExplosionsPoints (explosions map)) S_explosion sgrExplosion
    displayPoint (door map) S_door sgrDoor 
    displayPoint  (player map) S_player [Reset] 
    displayTimer configs time
    hFlush stdout
    where
        sgrBox = [SetColor Foreground Dull Yellow]
        sgrBomb = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid White]
        sgrExplosion = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
        sgrKey = [SetColor Foreground Vivid Yellow]
        sgrDoor = [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Cyan]

