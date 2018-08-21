-- tamanho lista
tam [] = 0
tam (x:xs) = 1 + tam xs

-- soma elementos lista
soma_lista [] = 0
soma_lista (x:xs) = x + soma_lista xs

-- soma elementos pares lista
par x = mod x 2 == 0

soma_par [] = 0
soma_par (x:xs)
  | par x = x + soma_par xs
  | otherwise = soma_par xs

-- soma elementos posicoes pares
soma_pos_par [] = 0
soma_pos_par [x] = 0
soma_pos_par (x:xs) = (head xs) + (soma_pos_par (tail xs))

-- existe item lista
existe item [] = False
existe item (x:xs)
  | x==item = True
  | existe item xs = True
  | otherwise = False

-- posicao item lista
posicao item [] = 0
posicao item (x:xs)
  | x==item = 1
  | (posicao item xs /= 0) = 1 + posicao item xs
  | otherwise = 0

-- conta repeticao item
repete item [] = 0
repete item (x:xs)
  | x==item = 1 + repete item xs
  | otherwise = repete item xs

-- maior elemento lista
maior [] = error "empty list"
maior [x] = x
maior (x:xs)
  | x >= maior xs = x
  | otherwise = maior xs

-- reverte lista
reverte [] = []
reverte (x:xs) = reverte xs ++ [x]

-- intercala lista 1
intercala1 [] [] = []
intercala1 x [] = x
intercala1 [] y = []
intercala1 (x:xs) (y:ys) = x:y:intercala1 xs ys

-- intercala lista 2
intercala2 [] [] = []
intercala2 x [] = []
intercala2 [] y = y
intercala2 (x:xs) (y:ys) = x:y:intercala2 xs ys

-- lista jah ordenada
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- gera lista 1 a n
gera_lista 0 = []
gera_lista x = gera_lista (x-1) ++ [x]

-- ultimo elemento lista
ultimo [] = error "empty list"
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- lista sem ultimo
inicio [] = []
inicio [x] = []
inicio (x:xs) = x : inicio xs

-- shift right
shiftr [] = []
shiftr [x] = [x]
shiftr (x:xs) = head (shiftr xs) : x : tail (shiftr xs)

-- shift right n vezes
shiftr_n 0 x = x
shiftr_n n x = shiftr_n (n-1) (shiftr x)

-- shift left
shiftl [] = []
shiftl (x:xs) = xs ++ [x]

-- shift left n vezes
shiftl_n 0 x = x
shiftl_n n x = shiftl_n (n-1) (shiftl x)

-- remove item 1 vez
remove1 item [] = []
remove1 item (x:xs)
  | x==item = xs
  | otherwise = x : remove1 item xs

-- remove item toda vez
remove item [] = []
remove item (x:xs)
  | x==item = remove item xs
  | otherwise = x : remove item xs

-- remove item lista n vezes
remove_n item n [] = []
remove_n item 0 x = x
remove_n item n (x:xs)
  | x==item = remove_n item (n-1) xs
  | otherwise = x : remove_n item n xs

-- remove item ultima vez aparece
remove_ult item [] = []
remove_ult item x
  | (last x)==item = init x
  | otherwise = (remove_ult item (init x)) ++ [last x]

-- troca velho por novo 1 vez
troca1 velho novo [] = []
troca1 velho novo (x:xs)
  | x==velho = novo : xs
  | otherwise = x : troca1 velho novo xs

-- troca velho por novo
troca velho novo [] = []
troca velho novo (x:xs)
  | x==velho = novo : troca velho novo xs
  | otherwise = x : troca velho novo xs

-- troca velho por novo n vezes
troca_n velho novo n [] = []
troca_n velho novo 0 x = x
troca_n velho novo n (x:xs)
  | x==velho = novo : troca_n velho novo (n-1) xs
  | otherwise = x : troca_n velho novo n xs
