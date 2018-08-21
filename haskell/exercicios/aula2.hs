-- soma elementos lista
soma_lista lista = soma_lista' lista 0
  where soma_lista' [] acc = acc
        soma_lista' (x:xs) acc = soma_lista' xs (x+acc)

-- posicao item lista
posicao item lista = posicao' item lista 0
  where posicao' _ [] _ = 0
        posicao' item (x:xs) acc
          | x==item = acc + 1
          | otherwise = posicao' item xs (acc+1)

-- maior elemento lista
maior [] = error "empty list"
maior [x] = x
maior (x:xs)
  | x >= mm = x
  | otherwise = mm
  where mm = maior xs

-- reverte lista
reverte lista = reverte' lista []
  where reverte' [] acc = acc
        reverte' (x:xs) acc = reverte' xs (x:acc)

-- gera lista 1 a n
gera_lista 0 = []
gera_lista n = gera_lista' n 1
  where gera_lista' 1 acc = [acc]
        gera_lista' n acc = acc:(gera_lista' (n-1) (acc+1))

-- todas as posicoes item lista
posicoes item lista = posicoes' item lista 1
  where posicoes' _ [] _ = []
        posicoes' item (x:xs) acc
          | x==item = acc:posicoes' item xs (acc+1)
          | otherwise = posicoes' item xs (acc+1)

-- split
split lista item = split' lista item ""
  where split' "" _ acc = [reverte acc]
        split' (x:xs) item acc
          | x==item = [reverte acc]++[xs]
          | otherwise = split' xs item (x:acc)

-- splitall
splitall lista item = splitall' lista item ""
  where splitall' "" _ acc = [reverte acc]
        splitall' (x:xs) item acc
          | x==item = (reverte acc):(splitall' xs item "")
          | otherwise = splitall' xs item (x:acc)

-- drop n primeiros lista
drop_n n [] = []
drop_n 0 lista = lista
drop_n n (x:xs) = drop_n (n-1) xs

-- take n primeiros lista
take_n n lista = take_n' n lista []
  where take_n' 0 lista acc = reverte acc
        take_n' _ [] _ = []
        take_n' n (x:xs) acc = take_n' (n-1) xs (x:acc)
