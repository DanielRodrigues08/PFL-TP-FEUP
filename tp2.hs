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
pascal n = [[binom x k | k <- [0 .. x]] | x <- [0..n]]