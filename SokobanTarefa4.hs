module SokobanTarefa4 where

tarefa4 :: [String] -> Char -> [String]
tarefa4 x cmd = m ++ reverteCoord (alteraCoords m (coordTuplo c) cmd)
            where (m,c) = retiraEspacos x []

reverteCoord :: [(Int,Int)] -> [String]
reverteCoord [] = []
reverteCoord ((a,b):xs) = (show a ++ " " ++ show b):reverteCoord xs

{-| Recebe a lista das coordenadas e converte-as em tuplos de inteiros -}
coordTuplo :: [String] -> [(Int,Int)]
coordTuplo [] = []
coordTuplo (x:xs) = (((read (head (words x)) :: Int),(read (last (words x)) :: Int)) : coordTuplo xs)

retiraEspacos :: [String] -> [String] -> ([String],[String])
retiraEspacos [] n = split (reverse n)
retiraEspacos (x:xs) n
    | words x == [] = retiraEspacos xs n
    | otherwise = retiraEspacos xs (x:n)

split :: [String] -> ([String],[String])
split [] = ([],[])
split (h:t) | elem '#' h = (h:mapa,coord)
            | otherwise = (mapa,h:coord)
        where (mapa,coord) = split t

{-| Recebe o mapa e as coordenadas de um ponto, e devolve o carater que se encontra nesse ponto. Caso seja um ponto fora dos limites do mapa, devolve um cardinal.-}
obtemChar :: [String] -> (Int,Int) -> Char
obtemChar mapa (x,y) 
    | x < 0 || y < 0 || x >= ((length (head mapa))-1) || y >= ((length mapa)-1) = '#'
    | otherwise = (mapa !! (y)) !! (x)

procuraCoord :: (Int,Int) -> [(Int,Int)] -> Int -> Int
procuraCoord _ [] _ = error "Coordenada não encontrada"
procuraCoord x xs n
    | x == head xs = n
    | otherwise = procuraCoord x (tail xs) (n+1)

alteraCoords :: [String] -> [(Int,Int)] -> Char -> [(Int,Int)]
alteraCoords m c cmd
    | cmd == 'U' = moveUp (reverse m) c
    | cmd == 'D' = moveDown (reverse m) c
    | cmd == 'L' = moveLeft (reverse m) c
    | cmd == 'R' = moveRight (reverse m) c
    | otherwise = c

moveUp :: [String] -> [(Int,Int)] -> [(Int,Int)]
moveUp m c
    | obtemChar m (x,(y+1)) == '#' = c
    | elem (x,(y+1)) c = if obtemChar m (x,(y+2)) /= '#' && not(elem (x,(y+2)) c) then reverse (moveCaixaBonecoUp c (procuraCoord (x,y+1) c 0) [] 1) else c
    | otherwise = [(x,y+1)] ++ (tail c)
        where (x,y) = head c

moveCaixaBonecoUp :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int -> [(Int,Int)]
moveCaixaBonecoUp [] _ c _ = c
moveCaixaBonecoUp (x:xs) n c b
    | b == 1 || n == 0 = moveCaixaBonecoUp xs (n-1) ((fst x,1 + snd x):c) 0
    | otherwise = moveCaixaBonecoUp xs (n-1) (x:c) 0

moveDown :: [String] -> [(Int,Int)] -> [(Int,Int)]
moveDown m c
    | obtemChar m (x,(y-1)) == '#' = c
    | elem (x,(y-1)) c = if obtemChar m (x,(y-2)) /= '#' && not(elem (x,(y-2)) c) then reverse (moveCaixaBonecoDown c (procuraCoord (x,y-1) c 0) [] 1) else c
    | otherwise = [(x,y-1)] ++ (tail c)
        where (x,y) = head c

moveCaixaBonecoDown :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int -> [(Int,Int)]
moveCaixaBonecoDown [] _ c _ = c
moveCaixaBonecoDown (x:xs) n c b
    | b == 1 || n == 0 = moveCaixaBonecoDown xs (n-1) ((fst x,(snd x)-1):c) 0
    | otherwise = moveCaixaBonecoDown xs (n-1) (x:c) 0
    
moveLeft :: [String] -> [(Int,Int)] -> [(Int,Int)]
moveLeft m c
    | obtemChar m (x-1,y) == '#' = c
    | elem (x-1,y) c = if obtemChar m (x-2,y) /= '#' && not(elem ((x-2),y) c) then reverse (moveCaixaBonecoLeft c (procuraCoord (x-1,y) c 0) [] 1) else c
    | otherwise = [(x-1,y)] ++ (tail c)
        where (x,y) = head c

moveCaixaBonecoLeft :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int -> [(Int,Int)]
moveCaixaBonecoLeft [] _ c _ = c
moveCaixaBonecoLeft (x:xs) n c b
    | b == 1 || n == 0 = moveCaixaBonecoLeft xs (n-1) ((((fst x)-1),snd x):c) 0
    | otherwise = moveCaixaBonecoLeft xs (n-1) (x:c) 0
    
moveRight :: [String] -> [(Int,Int)] -> [(Int,Int)]
moveRight m c
    | obtemChar m (x+1,y) == '#' = c
    | elem (x+1,y) c = if obtemChar m (x+2,y) /= '#' && not(elem ((x+2),y) c) then reverse (moveCaixaBonecoRight c (procuraCoord (x+1,y) c 0) [] 1) else c
    | otherwise = [(x+1,y)] ++ (tail c)
        where (x,y) = head c

moveCaixaBonecoRight :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int -> [(Int,Int)]
moveCaixaBonecoRight [] _ c _ = c
moveCaixaBonecoRight (x:xs) n c b
    | b == 1 || n == 0 = moveCaixaBonecoRight xs (n-1) ((1+fst x,snd x):c) 0
    | otherwise = moveCaixaBonecoRight xs (n-1) (x:c) 0
