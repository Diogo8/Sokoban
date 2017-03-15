module Main where

import SokobanTarefa4
import SokobanTarefa2
import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

main = do 
    inp <- getContents
    joga (inStr inp) chamaDesenhaMapa reageEvento

chamaDesenhaMapa :: [String] -> Picture
chamaDesenhaMapa x = (Pictures (fimJogo (tarefa2 x) (tarefa2 x)))

centrar :: [String] -> Picture -> Picture
centrar m pic = (Translate (-(toEnum x / 2)) (-(toEnum y / 2)) pic)
                    where (x,y) = ((length (head m)),(length m))

fimJogo :: [String] -> [String] -> [Picture]
fimJogo ((x:xs):ys) n 
    | x == 'H' = desenhaMapa n (1,1) 0
    | otherwise = fimJogo (xs:ys) n
fimJogo ([]:ys) n = fimJogo ys n
fimJogo [] n = desenhaMapa n (1,1) 1

desenhaMapa :: [String] -> (Float,Float) -> Int -> [Picture]
desenhaMapa _ _ 1 = [Translate (-500) 0 (Text "FIM DO JOGO")]
desenhaMapa [] _ 0 = []
desenhaMapa ((x:[]):[]) (x1,y1) 0
    | x == ' ' = (Translate (x1*20) (y1*20) Blank):desenhaMapa [] ((x1+1),y1) 0
    | x == '#' = (Translate (x1*20) (y1*20) (color yellow (rectangleSolid 20 20))):desenhaMapa [] ((x1+1),y1) 0
    | x == 'H' = (Translate (x1*20) (y1*20) (color green (rectangleSolid 20 20))):desenhaMapa [] ((x1+1),y1) 0
    | x == '.' = (Translate (x1*20) (y1*20) (color cyan (rectangleSolid 20 20))):desenhaMapa [] ((x1+1),y1) 0
    | x == 'I' = (Translate (x1*20) (y1*20) (color red (rectangleSolid 20 20))):desenhaMapa [] ((x1+1),y1) 0
    | x == 'o' = (Translate (x1*20) (y1*20) (color blue (rectangleSolid 20 20))):desenhaMapa [] ((x1+1),y1) 0
desenhaMapa ((x:[]):ys) (x1,y1) 0
    | x == ' ' = (Translate (x1*20) (y1*20) Blank):desenhaMapa (ys) (1,(y1+1)) 0
    | x == '#' = (Translate (x1*20) (y1*20) (color yellow (rectangleSolid 20 20))):desenhaMapa (ys) (1,(y1+1)) 0
    | x == 'H' = (Translate (x1*20) (y1*20) (color green (rectangleSolid 20 20))):desenhaMapa (ys) (1,(y1+1)) 0
    | x == '.' = (Translate (x1*20) (y1*20) (color cyan (rectangleSolid 20 20))):desenhaMapa (ys) (1,(y1+1)) 0
    | x == 'I' = (Translate (x1*20) (y1*20) (color red (rectangleSolid 20 20))):desenhaMapa (ys) (1,(y1+1)) 0
    | x == 'o' = (Translate (x1*20) (y1*20) (color blue (rectangleSolid 20 20))):desenhaMapa (ys) (1,(y1+1)) 0
desenhaMapa ((x:xs):ys) (x1,y1) 0
    | x == ' ' = (Translate (x1*20) (y1*20) Blank):desenhaMapa ((xs):ys) ((x1+1),y1) 0
    | x == '#' = (Translate (x1*20) (y1*20) (color yellow (rectangleSolid 20 20))):desenhaMapa ((xs):ys) ((x1+1),y1) 0
    | x == 'H' = (Translate (x1*20) (y1*20) (color green (rectangleSolid 20 20))):desenhaMapa ((xs):ys) ((x1+1),y1) 0
    | x == '.' = (Translate (x1*20) (y1*20) (color cyan (rectangleSolid 20 20))):desenhaMapa ((xs):ys) ((x1+1),y1) 0
    | x == 'I' = (Translate (x1*20) (y1*20) (color red (rectangleSolid 20 20))):desenhaMapa ((xs):ys) ((x1+1),y1) 0
    | x == 'o' = (Translate (x1*20) (y1*20) (color blue (rectangleSolid 20 20))):desenhaMapa ((xs):ys) ((x1+1),y1) 0


reageEvento :: Event -> [String] -> [String]
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) x = (tarefa4 x 'U')
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) x = (tarefa4 x 'D')
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) x = (tarefa4 x 'L')
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) x = (tarefa4 x 'R')
reageEvento _ x = x

-- | Não reage ao passar do tempo.
reageTempo :: Float -> mundo -> mundo
reageTempo t m = m

-- | Função que cria um jogo.
joga :: mundo -> (mundo -> Picture) -> (Event -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage = play
    (InWindow "Sokoban" (1000, 1000) (10, 10)) -- Tamanho da janela do jogo
    (greyN 0.5) -- Côr do fundo da janela
    45 -- refresh rate
    mapaInicial -- mapa inicial
    desenha -- função que desenha o mapa
    reage -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    reageTempo