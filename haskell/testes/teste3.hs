data Tree a = Vazia | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

-- insere um item numa abb
insereArvore :: (Ord a, Eq a) => Tree a -> a -> Tree a
insereArvore Vazia x = No x Vazia Vazia
insereArvore (No a esq dir) x
  | a == x = No a esq dir
  | a < x = No a esq (insereArvore dir x)
  | a > x = No a (insereArvore esq x) dir

-- converte uma lista em uma abb
inserelista :: (Ord a, Eq a) => [a] -> Tree a
inserelista a = inserelista' a Vazia
  where inserelista' [] arv = arv
        inserelista' (x:xs) arv = inserelista' xs (insereArvore arv x)
