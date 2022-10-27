maxpos :: [Int] -> Int
maxpos l = head (foldl (\acc x -> if x > head acc then x : acc else acc ++ [x]) [0] l)

dups :: [a] -> [a]
dups l = dupsAux l True

dupsAux :: [a] -> Bool -> [a]
dupsAux [] _ = []
dupsAux (x : xs) True = x : x : dupsAux xs False
dupsAux (x : xs) False = x : dupsAux xs True

transforma :: String -> String
transforma "" = ""
transforma (x : xs) = if (elem x "aeiou") then x : 'p' : x : transforma xs else x : transforma xs

type Vector = [Int]

type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta m1 = [[x !! i | x <- m1] | i <- [0 .. a]]
  where
    a = length (head m1) - 1

prodInterno :: Vector -> Vector -> Int
prodInterno v1 v2 = sum $ zipWith (*) v1 v2

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [[prodInterno x y | y <- transposta m2] | x <- m1]

data Arv a = F | N a (Arv a) (Arv a) deriving (Show)

getNo :: Arv Int -> Int
getNo (N a _ _) = a
getNo F = 0

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N _ l r) = N (1 + max (getNo (alturas l)) (getNo (alturas r))) (alturas l) (alturas r)
