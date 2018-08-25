-- separa string atraves de uma string delimitadora
-- splitseq :: (Eq a) => [a] -> [a] -> [[a]]

splitseq c l = splitseq' c l ""
  where splitseq' _ "" acc = [reverte acc]
        splitseq' c (x:xs) acc
          | c==take_n size (x:xs) = (reverte acc):splitseq' c (drop_n size (x:xs)) ""
          | otherwise = splitseq' c xs (x:acc)
          where size = len c

-- reverte lista
reverte lista = reverte' lista []
  where reverte' [] acc = acc
        reverte' (x:xs) acc = reverte' xs (x:acc)

-- drop n primeiros lista
drop_n n [] = []
drop_n 0 lista = lista
drop_n n (x:xs) = drop_n (n-1) xs

-- take n primeiros lista
take_n n lista = take_n' n lista []
  where take_n' 0 lista acc = reverte acc
        take_n' _ [] _ = []
        take_n' n (x:xs) acc = take_n' (n-1) xs (x:acc)

-- tamanho lista
len [] = 0
len (_:xs) = 1 + len xs
