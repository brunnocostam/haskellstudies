import Prelude

answer :: Int
answer = 42

difList :: [Int] -> [Int] -> [Int]
difList [] [] = []
difList a b = [e | e <- a, not (elem e b)]

interList :: [Int] -> [Int] -> [Int]
interList [] [] = []
interList a b = [e | e <- a, (elem e b)]

unionList :: [Int] -> [Int] -> [Int]
unionList [] [] = []
unionList a b = [e | e <- a ++ b]

unionListNoRep :: [Int] -> [Int] -> [Int]
unionListNoRep [] [] = []
unionListNoRep a b = removerRep (unionList a b)
                                 where removerRep [] = []
                                       removerRep (a:as) | elem a as = removerRep as
                                                         | otherwise = a : removerRep as

returnLast :: [Int] -> Int
returnLast [] = 0
returnLast (x:xs) | length (x:xs) == 1 = x
                  | otherwise = returnLast xs

returnNth :: [Int] -> Int -> Int
returnNth a 1 = head a
returnNth (a:as) n | n > 1 = returnNth as (n-1)

ultimoElemento [a] = a
ultimoElemento (a:as) = ultimoElemento as

revertList :: [Int] -> [Int]
revertList [] = []
revertList (a:as) = (revertList as) ++ [a]

decrescenteList :: [Int] -> [Int]
decrescenteList [] = []
decrescenteList (a:as) = decrescenteList [e | e <- as, e > a] ++ [a] ++ decrescenteList [e | e <- as, e < a]

isDec :: [Int] -> Bool
isDec x = x == (decrescenteList x)

isDec2 :: [Int] -> Bool
isDec2 [a] = True
isDec2 (a:b:xs) = a > b && (isDec xs)


fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z [] = z
fold f z (x:xs) = f x (foldr f z xs)

isDecF :: (a -> b -> b) -> b -> [a] -> Bool
isDecF _ _ [] = True
isDecF f z xs = fold (&&) (map (\(a,b) -> a>=b) (zip xs (tail xs)))

-- ehDescrescente3 lista =  fold (&&) (map (\(a,b) -> a>=b) (zip lista (tail lista)) )