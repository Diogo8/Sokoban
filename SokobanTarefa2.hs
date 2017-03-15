{-|
Module : Main
Description : Modulo Haskell com o objetivo de produzir o mapa de jogo.
Copyright : David Reis <a77989@alunos.uminho.pt>;
            Diogo Isidoro <a78356@alunos.uminho.pt>

Este modulo recebe um mapa e um conjunto de coordenadas, ambos validos, e processa-os de modo a formar um mapa sem carateres desnecessários e com as caixas e o boneco inseridos nele, a partir das coordenadas.
-}

module SokobanTarefa2 where

{-| Recebe o mapa e as coordenadas e devolve o mapa final com todas as alterações feitas. Separam-se as coordenadas do resto do mapa atraves do split, e o mapa é invertido para que a ordem das linhas na lista fique de acordo com o referencial. -}
tarefa2 :: [String] -> [String]
tarefa2 x = transformaCoords (reverse m,retiraEspacos c []) []
            where (m,c) = split x

{-| Recebe uma lista de strings e remove aquelas que estao vazias ou sao compostas unicamente por espaços.-}
retiraEspacos :: [String] -> [String] -> [String]
retiraEspacos [] n = n
retiraEspacos (x:xs) n
    | words x == [] = retiraEspacos xs n
    | otherwise = retiraEspacos xs (x:n)

{-| Recebe o mapa e as coordenadas numa lista de String e separa-os, devolvendo um tuplo constituido pela lista das Strings do mapa e a lista de Strings das coordenadas. -}
split :: [String] -> ([String],[String])
split [] = ([],[])
split (h:t) | elem '#' h = (h:mapa,coord)
            | otherwise = ([],h:t)
        where (mapa,coord) = split t

{-| Recebe o tuplo constituido pelo mapa e pelas coordenadas e o conjunto vazio e passa as coordenadas da forma String para um tuplo do tipo (x,y) para cada coordenada.-}
transformaCoords :: ([String],[String]) -> [(Int,Int)] -> [String]
transformaCoords (z,[]) y = processaMapa (z,y) 1
transformaCoords (z,(x:xs)) y = transformaCoords (z,xs) (((read (head (words x)) :: Int), (read (last (words x))::Int)):y)

{-| Recebe o mapa e as coordenadas e devolve apenas o mapa, usando as coordenadas para colocar o boneco e a caixa nos respetivo sitios. Recebe tambem um inteiro que transmite à função alteraLinha para indicar se a coordenada é a de uma caixa ou a do boneco.-}
processaMapa :: ([String],[(Int,Int)]) -> Int -> [String]
processaMapa (z,((x,y):xs)) 1 = processaMapa (((take y z) ++ [(alteraLinha (z!!y) x 1)] ++ (drop (y+1) z)), xs) 0
processaMapa (z,[]) _ = removeCardinais z (1,1) z
processaMapa (z,((x,y):xs)) _ = processaMapa (((take y z) ++ [(alteraLinha (z!!y) x 0)] ++ (drop (y+1) z)), xs) 0

{-| Recebe uma linha, um inteiro que indica o sítio onde vai ser colocada a caixa ou o boneco, e recebe um inteiro que indica se a coordenada recebida é a do boneco ou a de uma caixa. Devolve a linha com as alteracoes devidas.-}
alteraLinha :: String -> Int -> Int -> String
alteraLinha z n f
    | f == 1 = (take n z) ++ "o" ++ (drop (n+1) z)
    | (f /= 1) && (z!!n == '.') = (take n z) ++ "I" ++ (drop (n+1) z)
    | otherwise = (take n z) ++ "H" ++ (drop (n+1) z)

{-| Recebe o mapa (já com as coordenadas substituidas), recebe um tuplo que é a coordenada a avaliar, e recebe novamente o mapa, que vai sempre manter inalterado ao longo da função (serve apenas para a verificacao da importância de um determinado cardinal. Avalia a presenca de cardinais à volta daquele que recebeu como coordenada, e devolve o mapa sem cardinais desnecessarios, que é o resultado final do módulo.-}
removeCardinais :: [String] -> (Int,Int) -> [String] -> [String]
removeCardinais x (z,y) p
    | z > length (head p) && y >= (length p) = x 
    | z > length (head p) && y < (length p) = removeCardinais x (1,(y+1)) p
    | otherwise = if obtemChar p (z+1,y) == '#' 
                     && obtemChar p (z-1,y) == '#'
                     && obtemChar p (z-1,y-1) == '#'
                     && obtemChar p (z, y-1) == '#'
                     && obtemChar p (z+1, y-1) == '#'
                     && obtemChar p (z+1, y+1) == '#'
                     && obtemChar p (z, y+1) == '#'
                     && obtemChar p (z-1, y+1) == '#' then removeCardinais (alteraMapaCard x z y) (z+1,y) p 
                                                      else removeCardinais x (z+1,y) p

{-| Recebe o mapa e as coordenadas do cardinal a remover, envia a linha e a posicao a alterar para a funcao alteraLinhaCard e devolve o mapa com as alteracoes feitas.-}
alteraMapaCard :: [String] -> Int -> Int -> [String]
alteraMapaCard mapa x y = (take (y-1) mapa) ++ [alteraLinhaCard (mapa!!(y-1)) x] ++ (drop y mapa)

{-| Recebe a linha e a posicao do cardinal a remover, e devolve a linha com o cardinal removido. -}
alteraLinhaCard :: String -> Int -> String
alteraLinhaCard linha x = (take (x-1) linha) ++ " " ++ (drop x linha)

{-| Recebe o mapa e as coordenadas de um ponto, e devolve o carater que se encontra nesse ponto. Caso seja um ponto fora dos limites do mapa, devolve um cardinal.-}
obtemChar :: [String] -> (Int,Int) -> Char
obtemChar mapa (x,y) 
    | x < 1 || y < 1 || x >= (length (head mapa)) || y >= (length mapa) = '#'
    | otherwise = (mapa !! (y-1)) !! (x-1)
