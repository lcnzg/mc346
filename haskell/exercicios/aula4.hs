data Tree a = Vazia | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

exemplo =
    No 25
        (No 20
            (No 10
                (No 1 Vazia Vazia)
                (No 12 Vazia Vazia)
            )
            (No 22
                (No 21 Vazia Vazia)
                (No 23 Vazia Vazia)
            )
        )
        (No 36
            (No 30
                (No 28 Vazia Vazia)
                (Vazia)
            )
            (No 48
                (No 45 Vazia Vazia)
                (No 50 Vazia Vazia)
            )
        )

-- acha um item buma arvore de busca binaria
contem :: (Ord a) => (Tree a) -> a -> Bool
contem Vazia _ = False
contem (No a esq dir) x
  | x == a = True
  | x < a = contem esq x
  | x > a = contem dir x

-- verifica se uma arvore é um abb
ehABB :: (Ord a) => (Tree a) -> Bool
ehABB Vazia = True
ehABB (No a esq dir)
  | a < minimo = False
  | a > maximo = False
  | otherwise = (ehABB esq) && (ehABB dir)
  where minimo = mais_esq (No a esq dir)
        maximo = mais_dir (No a esq dir)

-- insere um item numa abb
insere :: (Ord a) => Tree a -> a -> Tree a
insere Vazia x = No x Vazia Vazia
insere (No a esq dir) x
  | a == x = No a esq dir
  | a < x = No a esq (insere dir x)
  | a > x = No a (insere esq x) dir

-- remove um item de uma abb
remove :: (Ord a) => Tree a -> a -> Tree a
remove Vazia _ = Vazia
remove (No a esq dir) x
  | x == a = remove_raiz (No a esq dir)
  | x < a = No a (remove esq x) dir
  | x > a = No a esq (remove dir x)

-- remove raiz
remove_raiz :: (Ord a) => Tree a -> Tree a
remove_raiz (No a Vazia dir) = dir
remove_raiz (No a esq Vazia) = esq
remove_raiz (No a esq dir) = (No v esq dir)
  where
    v = mais_esq dir

-- min: retorna o no mais a esquerda
mais_esq :: (Ord a) => Tree a -> a
mais_esq (No a Vazia _) = a
mais_esq (No _ esq _) = mais_esq esq

-- max: retorna o no mais a direita
mais_dir :: (Ord a) => Tree a -> a
mais_dir (No a _ Vazia) = a
mais_dir (No _ _ dir) = mais_dir dir

-- calcula a profundidade maxima de uma abb
profundidade :: Tree a -> Int
profundidade Vazia = 0
profundidade (No a esq dir) = 1 + max (profundidade esq) (profundidade dir)

-- coverte uma abb numa lista em ordem infixa (arvore-esquerda, no, arvore-direita)
infixa :: Tree a -> [a]
infixa Vazia = []
infixa (No a esq dir) = infixa esq ++ [a] ++ infixa dir

-- converte uma abb numa lista em ordem prefixa (no, ae, ad)
preordem :: (Ord a) => Tree a -> [a]
preordem Vazia = []
preordem (No a esq dir) = [a] ++ preordem esq ++ preordem dir

-- converte uma lista em uma abb
gera_arvore :: (Ord a) => [a] -> Tree a
gera_arvore a = gera_arvore' a Vazia
  where gera_arvore' [] arv = arv
        gera_arvore' (x:xs) arv = gera_arvore' xs (insere arv x)
