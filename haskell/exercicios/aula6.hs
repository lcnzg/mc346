-- reimplemente os exercicios da aula 1 usando as funcoes de alto nivel
-- (map, filter, fold)

-- tamanho lista
tam lista = foldr (\ _ x -> 1+x) 0 lista

-- soma elementos lista
soma_lista lista = foldl (+) 0 lista

-- soma elementos pares lista
soma_par lista = foldl (+) 0 (filter (\ x -> x `mod` 2 == 0) lista)

-- soma elementos posicoes pares ?
soma_pos_par l = foldl (\acc (x,y) -> if mod x 2 == 0 then acc+y else acc) 0 $ zip [1..length l] l

-- existe item lista
existe item lista = foldr (\ x bool -> (x==item) || bool) False lista

-- posicao item lista ?

-- conta repeticao item
repete item lista = foldr (\ _ x -> 1+x) 0 (filter (\ x -> x==item) lista)

-- maior elemento lista ?

-- reverte lista
reverte lista = foldr (\ a b -> b ++ [a]) [] lista

-- intercala lista 1 ?

-- intercala lista 2 ?

-- lista jah ordenada ?

-- gera lista 1 a n ?

-- ultimo elemento lista ?

-- lista sem ultimo ?

-- shift right ?

-- shift right n vezes ?

-- shift left ?

-- shift left n vezes ?

-- remove item 1 vez (removendo do final)
remove1 item lista = remove1' (foldr (\ x (removeu, acc) -> if ((x==item) && removeu) then (False, acc) else (removeu, x:acc)) (True,[]) lista)
  where remove1' (_,x) = x

-- remove item toda vez
remove item lista = filter (\ x -> x/=item) lista

-- remove item lista n vezes (removendo do final)
remove_n item n lista = remove_n' (foldr (\ x (n, acc) -> if ((x==item) && n/=0) then ((n-1), acc) else (n, x:acc)) (n,[]) lista)
  where remove_n' (_,x) = x

-- remove item ultima vez aparece
remove_ult item lista = remove_ult' (foldr (\ x (removeu, acc) -> if ((x==item) && removeu) then (False, acc) else (removeu, x:acc)) (True,[]) lista)
  where remove_ult' (_,x) = x

-- troca velho por novo 1 vez ?

-- troca velho por novo
troca velho novo lista = map (\ x -> if x==velho then novo else x) lista

-- troca velho por novo n vezes ?

-- transposta da matriz formada com listas de linhas
transposta ([]:_) = []
transposta mat = col1 : (transposta restmat)
  where col1 = map head mat
        restmat = map tail mat

-- multiplica 2 matrizes
matmul m1 m2 = matmul' m1 (transposta m2)
  where matmul' [] _ = [[]]
        matmul' (lin:r) m2 = (map (\l -> dotprod l lin) m2) : (matmul' r m2)
        dotprod l1 l2 = sum $ zipWith (*) l1 l2
