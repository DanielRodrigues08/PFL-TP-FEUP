{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant where" #-}
import Data.Char
import Data.Type.Coercion (trans)
import Language.Haskell.TH (lam1E)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

or' :: [Bool] -> Bool
or' xs = foldr (||) True xs

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n -1) x

(!?) :: [a] -> Int -> a
(!?) (x : _) 0 = x
(!?) (_ : xs) n = xs !? (n -1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' v (x : xs) = v == x || elem v xs

interperse' :: a -> [a] -> [a]
interperse' _ [] = []
interperse' _ [x] = [x]
interperse' a (x : xs) = x : a : interperse' a xs

mdc' :: Integer -> Integer -> Integer
mdc' a 0 = a
mdc' a b = mdc' b (mod a b)

insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x : xs)
  | a >= x = x : insert' a xs
  | otherwise = a : x : xs

isort' :: Ord a => [a] -> [a]
isort' xs = foldr insert' [] xs

min' :: Ord a => [a] -> a
min' [] = error "Empty List!"
min' [x] = x
min' (x : xs) = if x < min' xs then x else min' xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x : xs) = if x == a then xs else x : delete' a xs

ssort' :: Ord a => [a] -> [a]
ssort' [] = []
ssort' l = a : ssort' (delete' a l)
  where
    a = min' l

ex2_6 :: Int
ex2_6 = sum [x ^ 2 | x <- [1 .. 100]]

aprox :: Int -> Double
aprox n = 4 * sum [(-1) ^ x / fromIntegral (2 * x + 1) | x <- [0 .. n]]

aprox' :: Int -> Double
aprox' n = sqrt $ 12 * sum [(-1) ^ x / fromIntegral ((x + 1) ^ 2) | x <- [0, 100 .. n]]

dotprod :: [Float] -> [Float] -> Float
dotprod l1 l2 = sum [x * y | (x, y) <- zip l1 l2]

divprop :: Integer -> [Integer]
divprop n = [x | x <- [1 .. n -1], mod n x == 0]

perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1 .. n -1], sum (divprop x) == x]

pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = concat [[(x, y, z), (y, x, z)] | x <- [1 .. n], y <- [x .. n], let z = (round . sqrt . fromIntegral) (x ^ 2 + y ^ 2), z <= n, x ^ 2 + y ^ 2 == z ^ 2]

primo :: Integer -> Bool
primo n = length (divprop n) == 1

mersennes :: [Int]
mersennes = [fromIntegral z | n <- [1 .. 30], let z = 2 ^ n - 1, primo z]

binom :: Integer -> Integer -> Integer
binom n k = div (product [1 .. n]) (product [1 .. k] * product [1 .. n - k])

pascal :: Integer -> [[Integer]]
pascal n = [[binom x k | k <- [0 .. x]] | x <- [0 .. n]]

cifraLetra :: Int -> Char -> Char
cifraLetra n l
  | isUpper l = chr (mod (ord l + n - ord 'A') 26 + ord 'A')
  | isLower l = chr (mod (ord l + n - ord 'a') 26 + ord 'a')
  | otherwise = l

cifrar :: Int -> String -> String
cifrar n p = [cifraLetra n x | x <- p]

concat'' :: [[a]] -> [a]
concat'' l = [x | xs <- l, x <- xs]

replicate'' :: Int -> a -> [a]
replicate'' n a = [a | _ <- [1 .. n]]

forte :: String -> Bool
forte password = length password >= 8 && or [isUpper x | x <- password] && or [isLower x | x <- password] && or [isNumber x | x <- password]

mindiv :: Int -> Int
mindiv n
  | null z = n
  | otherwise = head z
  where
    z = [x | x <- [2 .. floor (sqrt (fromIntegral n))], mod n x == 0]

primo' :: Int -> Bool
primo' n = mindiv n == 1

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x : nub' [y | y <- xs, y /= x]

transpose' :: [[a]] -> [[a]]
transpose' l = [[x !! n | x <- l] | n <- [0 .. (z -1)]]
  where
    z = length (head l)

algarismos :: Int -> [Int]
algarismos n
  | n < 9 = [n]
  | otherwise = algarismos (div n 10) ++ [mod n 10]

toBits :: Int -> [Int]
toBits n
  | n <= 1 = [n]
  | otherwise = toBits (div n 2) ++ [mod n 2]

fromBits :: [Int] -> Int
fromBits l = sum [(2 ^ j) * i | (i, j) <- zip l (reverse [0 .. length l - 1])]

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] l2 = l2
merge l1 [] = l1
merge (x1 : l1) (x2 : l2)
  | x1 < x2 = x1 : merge l1 (x2 : l2)
  | otherwise = x2 : merge (x1 : l1) l2

metades :: [a] -> ([a], [a])
metades l = splitAt s l
  where
    s = div (length l) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [n] = [n]
msort l = merge (msort x1) (msort x2)
  where
    (x1, x2) = metades l
