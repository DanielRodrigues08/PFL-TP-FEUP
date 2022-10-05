import Language.Haskell.TH.Syntax (mk_unboxed_tup_name)

testeTriangulo :: Float -> Float -> Float -> Bool
testeTriangulo a b c = a + b > c && a + c > b && b + c > a

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

metades :: [a] -> ([a], [a])
metades l = splitAt s l
  where
    s = div (length l) 2

last' :: [a] -> a
last' l = last l

init' :: [a] -> [a]
init' l = take (length l - 1) l

binom :: Integer -> Integer -> Integer
binom n k = div (product [1 .. n]) (product [1 .. k] * product [1 .. n - k])

binom' :: Integer -> Integer -> Integer
binom' n k = if k < n - k then div (product [n - k + 1 .. n]) (product [1 .. k]) else div (product [k + 1 .. n]) (product [1 .. n - k])

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((- b + s) / 2, (- b - s) / 2)
  where
    s = sqrt (b ^ 2 - 4 * a * c)

classifica :: Int -> String
classifica n
  | n < 0 || n > 20 = "off limits"
  | n <= 9 && n >= 0 = "reprovado"
  | n >= 10 && n <= 12 = "sufeciente"
  | n >= 13 && n <= 15 = "bom"
  | n >= 16 && n <= 18 = "muito bom"
  | n >= 19 && n <= 20 = "muito bom com distinção"

classificaimc :: Float -> Float -> String
classificaimc peso altura
  | imc < 18.5 = "baixo pesso"
  | imc >= 18.5 && imc < 25 = "peso normal"
  | imc >= 25 && imc < 30 = "excesso de peso"
  | imc >= 30 = "obesidade"
  | otherwise = "erro"
  where
    imc = peso / altura ^ 2

max3 :: Ord a => a -> a -> a -> a
max3 x y z = max z (max x y)

min3 :: Ord a => a -> a -> a -> a
min3 x y z = min z (min x y)

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor True False = True
xor False True = True

safetail :: [a] -> [a]
safetail [] = []
safetail l = tail l

safetail' :: [a] -> [a]
safetail' l = if length l == 0 then [] else tail l

safetail'' :: [a] -> [a]
safetail'' l
  | length l == 0 = []
  | otherwise = tail l

curta :: [a] -> Bool
curta l = s >= 0 && s <= 3 where s = length l

mediana :: RealFloat a => a -> a -> a -> a
mediana x y z = x + y + z - max3 x y z - min3 x y z

