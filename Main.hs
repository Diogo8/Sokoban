module Main where

import SokobanTarefa4
import SokobanTarefa2
import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s
import System.Directory

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]


main = do 
    bor <- loadBMP "Bordas.bmp"
    arr <- loadBMP "Arrumos.bmp"
    cai <- loadBMP "Caixa.bmp"
    bon <- loadBMP "Boneco.bmp"
    caiArr <- loadBMP "CaixaArrumada.bmp"
    menu <- loadBMP "Menu.bmp"
    back <- loadBMP "Background.bmp"
    fim <- loadBMP "Fim.bmp"
    joga ([],2,bor,arr,cai,bon,menu,back,fim,0,caiArr,[]) chamaDesenhaMapa reageEvento

type Jogo = ([String],Int,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Int,Picture,[[String]])

chamaDesenhaMapa :: Jogo -> Picture
chamaDesenhaMapa (x,0,a,b,c,d,e,back,g,jog,h,hist)
    | jog < 10 = Pictures [(Scale 0.7 0.7 back), (centrar ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) (Pictures (fimJogo ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist)))),(Scale 0.18 0.18 (Translate (1430) (-1680) (color black (Text (show jog))))),color white (Translate (-300) (-265) (Scale 0.2 0.2 (Text "R- Restart")))]
    | jog >= 10 && jog < 100 = Pictures [(Scale 0.7 0.7 back), (centrar ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) (Pictures (fimJogo ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist)))),(Scale 0.18 0.18 (Translate (1400) (-1680) (color black(Text (show jog))))),color white(Translate (-300) (-265) (Scale 0.2 0.2 (Text "R- Restart")))]
    | otherwise = Pictures [(Scale 0.7 0.7 back), (centrar ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) (Pictures (fimJogo ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist) ((tarefa2 x),0,a,b,c,d,e,back,g,jog,h,hist)))),(Scale 0.16 0.16 (Translate (1530) (-1890) (color black (Text (show jog))))),color white (Translate (-300) (-265) (Scale 0.2 0.2 (Text "R- Restart")))]

chamaDesenhaMapa (x,n,a,b,c,d,e,back,g,jog,h,hist) = Pictures (desenhaMapa (x,n,a,b,c,d,e,back,g,jog,h,hist) (1,1))

centrar :: Jogo -> Picture -> Picture
centrar (m,0,a,b,c,d,e,f,g,jog,h,hist) pic = (Translate (-((toEnum x / 2)*32)) (-((toEnum y / 2)*32)) pic)
                    where (x,y) = ((length (head m)),(length m))

fimJogo :: Jogo -> Jogo -> [Picture]
fimJogo (((x:xs):ys),0,a,b,c,d,e,back,g,jog,z,hist) (o,on,h,i,j,k,l,m,n,jog1,w,hist1) 
    | x == 'H' = (desenhaMapa (o,0,a,b,c,d,e,back,g,jog,z,hist) (1,1))
    | otherwise = fimJogo ((xs:ys),0,a,b,c,d,e,back,g,jog,z,hist) (o,on,h,i,j,k,l,m,n,jog1,w,hist1)
fimJogo (([]:ys),0,a,b,c,d,e,f,g,jog,z,hist) (o,on,h,i,j,k,l,m,n,jog1,w,hist1)  = fimJogo (ys,0,a,b,c,d,e,f,g,jog,z,hist) (o,on,h,i,j,k,l,m,n,jog1,z,hist1)
fimJogo ([],0,a,b,c,d,e,f,g,jog,z,hist) (o,on,h,i,j,k,l,m,n,jog1,w,hist1)  = desenhaMapa (o,1,a,b,c,d,e,f,g,jog,z,[]) (1,1)

desenhaMapa :: Jogo -> (Float,Float) -> [Picture]
desenhaMapa (_,2,_,_,_,_,menu,_,_,_,_,_) _ = [(Scale 0.7 0.7 menu)]
desenhaMapa (m,1,_,_,_,_,_,_,fim,jog,_,[]) _
    | jog < 10 = [Translate ((toEnum x / 2)*32) ((toEnum y / 2)*32) (Pictures [(Scale 0.7 0.7 fim),Translate 90 20 (Scale 0.25 0.25 (color white (Text (show jog))))])]
    | jog > 10 && jog < 100 = [Translate ((toEnum x / 2)*32) ((toEnum y / 2)*32) (Pictures [(Scale 0.7 0.7 fim),Translate 80 20 (Scale 0.24 0.24 (color white (Text (show jog))))])]
    | otherwise = [Translate ((toEnum x / 2)*32) ((toEnum y / 2)*32) (Pictures [(Scale 0.7 0.7 fim),Translate 77 22 (Scale 0.175 0.175 (color white (Text (show jog))))])]
                where (x,y) = ((length (head m)),(length m))
desenhaMapa ([],0,_,_,_,_,_,_,_,_,_,_) _ = []
desenhaMapa (((x:[]):[]),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (x1,y1)
    | x == ' ' = (Translate (x1*32) (y1*32) Blank):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == '#' = (Translate (x1*32) (y1*32) bor):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'H' = (Translate (x1*32) (y1*32) cai):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == '.' = (Translate (x1*32) (y1*32) arr):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'I' = (Translate (x1*32) (y1*32) caiArr):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'o' = (Translate (x1*32) (y1*32) bon):desenhaMapa ([],0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
desenhaMapa (((x:[]):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (x1,y1)
    | x == ' ' = (Translate (x1*32) (y1*32) Blank):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
    | x == '#' = (Translate (x1*32) (y1*32) bor):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
    | x == 'H' = (Translate (x1*32) (y1*32) cai):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
    | x == '.' = (Translate (x1*32) (y1*32) arr):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
    | x == 'I' = (Translate (x1*32) (y1*32) caiArr):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
    | x == 'o' = (Translate (x1*32) (y1*32) bon):desenhaMapa (ys,0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (1,(y1+1))
desenhaMapa (((x:xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) (x1,y1)
    | x == ' ' = (Translate (x1*32) (y1*32) Blank):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == '#' = (Translate (x1*32) (y1*32) bor):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'H' = (Translate (x1*32) (y1*32) cai):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == '.' = (Translate (x1*32) (y1*32) arr):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'I' = (Translate (x1*32) (y1*32) caiArr):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)
    | x == 'o' = (Translate (x1*32) (y1*32) bon):desenhaMapa (((xs):ys),0,bor,arr,cai,bon,menu,back,fim,jog,caiArr,hist) ((x1+1),y1)

reageEvento :: Event -> Jogo -> Jogo
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'U') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'U'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist) 
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'D') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'D'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'L') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'L'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'R') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'R'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (Char 'w') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'U') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'U'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (Char 's') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'D') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'D'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (Char 'a') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'L') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'L'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (Char 'd') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if (tarefa4 x 'R') == x then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((tarefa4 x 'R'),0,a,b,c,d,e,f,g,(jog+1),h,x:hist)
reageEvento (EventKey (Char 'u') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if hist == [] then (x,0,a,b,c,d,e,f,g,jog,h,hist) else ((head hist),0,a,b,c,d,e,f,g,(jog+1),h,(drop 1 hist))
reageEvento (EventKey (Char 'r') Down _ _) (x,0,a,b,c,d,e,f,g,jog,h,hist) = if hist == [] then (x,0,a,b,c,d,e,f,g,0,h,hist) else ((last hist),0,a,b,c,d,e,f,g,0,h,[])
reageEvento (EventKey (Char '1') Down _ _) (x,2,a,b,c,d,e,f,g,jog,h,hist) = (mapaFacil,0,a,b,c,d,e,f,g,0,h,[])
reageEvento (EventKey (Char '2') Down _ _) (x,2,a,b,c,d,e,f,g,jog,h,hist) = (mapaMedio,0,a,b,c,d,e,f,g,0,h,[])
reageEvento (EventKey (Char '3') Down _ _) (x,2,a,b,c,d,e,f,g,jog,h,hist) = (mapaDificil,0,a,b,c,d,e,f,g,0,h,[])
reageEvento (EventKey (Char 'q') Down _ _) (x,_,a,b,c,d,e,f,g,jog,h,hist) = ([],2,a,b,c,d,e,f,g,0,h,[])
reageEvento _ (x,n,a,b,c,d,e,f,g,jog,h,hist) = (x,n,a,b,c,d,e,f,g,jog,h,hist)

mapaFacil :: [String]
mapaFacil = ["#######",
             "##   ##",
             "#    .#",
             "#    .#",
             "#     #",
             "#    ##",
             "#######",
             "1 2",
             "2 2",
             "2 3"]


mapaMedio :: [String]
mapaMedio = ["###################",
             "#####   ###########",
             "#####   ###########",
             "#####   ###########",
             "###      ##########",
             "### # ## ##########",
             "#   # ## #####  ..#",
             "#               ..#",
             "##### ### # ##  ..#",
             "#####     #########",
             "###################",
             "11 2",
             "5 8",
             "7 7",
             "5 6",
             "7 6",
             "2 3",
             "5 3"]


mapaDificil :: [String]
mapaDificil = ["###########",
               "######   ##",
               "##  ##   ##",
               "#   #    ##",
               "#       ###",
               "#. #  # ###",
               "#.#. #    #",
               "#...#   # #",
               "# ..      #",
               "#  ########",
               "###########",
               "9 3",
               "6 7",
               "6 8",
               "7 7",
               "4 5",
               "6 3",
               "7 3",
               "8 2",
               "8 4"]

-- | Não reage ao passar do tempo.
reageTempo :: Float -> mundo -> mundo
reageTempo t m = m

-- | Função que cria um Jogo.
joga :: mundo -> (mundo -> Picture) -> (Event -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage = play
    (InWindow "Sokoban" (700, 700) (1, 1)) -- Tamanho da janela do Jogo
    (greyN 0.7) -- Côr do fundo da janela
    45 -- refresh rate
    mapaInicial -- mapa inicial
    desenha -- função que desenha o mapa
    reage -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    reageTempo