import Data.Char

soma1 [] ch = [(ch,1)]
soma1 ((x,n):xs) ch
     | x==ch = (x,n+1):xs
     | otherwise = (x,n): soma1 xs ch

vogalmaiscomum l = snd $ maximum $ map fflip $ foldl soma1 [] $ filter (`elem` "aeiou") $ map toLower l
  where fflip (a,b) = (b,a)
