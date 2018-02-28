main :: IO ()
main = print $ decode' $ encodeCorrected $ encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

data Encode x = Single x | Plural (Int,x) deriving Show

encodeCorrected [] = []
encodeCorrected ((1,x):xs) = Single x : encodeCorrected xs
encodeCorrected ((num,x):xs) = Plural (num,x) : encodeCorrected xs


encode = foldr encodePom []

encodePom elem [] = [(1,elem)]
encodePom elem ((num,x):xs) = if x == elem
    then (num + 1,x):xs
    else (1,elem):(num,x):xs

decode' [] = []
decode' (Single x:xs) = x : decode' xs
decode' (Plural (num,x):xs) = [x | _ <- [1..num]] ++ decode' xs
