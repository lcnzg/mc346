-- troca velho por novo
troca velho novo [] = []
troca velho novo (x:xs)
  | x==velho = novo : troca velho novo xs
  | otherwise = x : troca velho novo xs
