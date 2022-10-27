import Data.List

half :: Fractional a => a -> a
half n = n / 2

xor :: Bool -> Bool -> Bool
xor b1 b2 = b1 /= b2

cbrt :: Floating a => a -> a
cbrt x = x ** (1 / 3)

heron :: Floating a => a -> a -> a -> a
heron a b c = (s * (s - a) * (s - b) * (s - c)) ** (1 / 2)
  where
    s = (a + b + c) / 2

mySwap :: (b, a) -> (a, b)
mySwap (b, a) = (a, b)

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (x1, y1) (x2, y2) = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** (1 / 2)

distanceInf :: (Num a, Ord a) => (a, a) -> (a, a) -> a
distanceInf (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

fib :: (Num a, Ord a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n
  | n > 0 = fib (n -2) + fib (n -1)
  | otherwise = error "negative argument"

ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m -1) 1
  | m > 0 && n > 0 = ackermann (m -1) (ackermann m (n -1))

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct l1 l2 = sum (zipWith (*) l1 l2)

seq22 :: Num a => Int -> [a]
seq22 n = 1 : replicate (n -2) 2 ++ [1]

seq42 :: Num a => Int -> [a]
seq42 n = 1 : take (n -2) (cycle [4, 2]) ++ [1]

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup l = a : myGroup (drop (length a) l)
  where
    a = takeWhile (\x -> x == head l) l

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : myZip3 xs ys zs

differentFromNext :: Eq a => [a] -> [a]
differentFromNext l
  | length l == 0 = []
  | length l == 1 = []
differentFromNext (x : y : xs) = if (x /= y) then x : differentFromNext (y : xs) else differentFromNext (y : xs)

lengthMaxElement :: Ord a => [[a]] -> Int
lengthMaxElement l = foldr1 (\x y -> if x >= y then x else y) (map length l)

myTransposeAux :: Ord a => Int -> [[a]] -> [a]
myTransposeAux _ [] = []
myTransposeAux n (x : xs) = if n >= length x then myTransposeAux n xs else x !! n : myTransposeAux n xs

myTranspose :: Ord a => [[a]] -> [[a]]
myTranspose l = [myTransposeAux i l | i <- [0 .. lengthMaxElement l - 1]]

conseqPairs :: [a] -> [(a, a)]
conseqPairs l = zip (init l) (tail l)

myZip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3' l1 l2 l3 = [(l1 !! i, l2 !! i, l3 !! i) | i <- [0 .. length l1 - 1]]

checkMod3ThenOdd :: [Int] -> Bool
checkMod3ThenOdd l = and [odd x | x <- l, mod x 3 == 0]

repeatNTimes :: Int -> [a] -> [a]
repeatNTimes n l = [x | x <- l, _ <- [1 .. n]]

myPermutation :: Eq a => [a] -> [[a]]
myPermutation [] = [[]]
myPermutation l = [h : t | h <- l, t <- myPermutation (delete h l)]

applyN :: (Integral n) => (a -> a) -> n -> a -> a
applyN _ 0 e = e

apllyN f n e = f (applyN f (n -1) e)

orderedTriples :: Ord a => [(a, a, a)] -> [(a, a, a)]
orderedTriples l1 = filter (\(x, y, z) -> x <= y && y <= z) l1

myMap :: (a -> a) -> [a] -> [a]
myMap f l = [f x | x <- l]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [x | x <- l, f x]

countVowels :: String -> Int
countVowels s = sum (map (\x -> if elem x "aeiou" then 1 else 0) s)

myMap' :: (a -> b) -> [a] -> [b]
myMap' f l = foldr (\x acc -> (f x) : acc) [] l

largePairs :: (Ord a, Num a) => a -> [(a, a)] -> [(a, a)]
largePairs n l = foldr (\x acc -> if (fst x + snd x >= n) then x : acc else acc) [] l

fuseDigits :: Num a => [a] -> a
fuseDigits l = foldl (\acc x -> acc * 10 + x) 0 l

separateSingleDigits :: (Integral a) => [a] -> ([a], [a])
separateSingleDigits l = foldr (\x (ys, ns) -> if (x >= 0 && x <= 9) then (x : ys, ns) else (ys, x : ns)) ([], []) l

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr _ acc [] = [acc]
myScanr f acc (x : xs) = (f x (head t)) : t
  where
    t = myScanr f acc xs

type Pair a = (a, a)

type Relation a = [Pair a]

isReflexive :: (Eq a) => Relation a -> Bool
isReflexive l1 = and [elem (snd x, fst x) l1 | x <- l1]

isTransitive :: (Eq a) => Relation a -> Bool
isTransitive l1 = and [if (snd x == fst y) then elem (fst x, snd y) l1 else True | x <- l1, y <- l1]

data Shape = Circle Double Double Double | Rectangle Double Double Double Double

perimeter :: Shape -> Double
perimeter (Circle _ _ z) = 2 * pi * z
perimeter (Rectangle x1 y1 x2 y2) = 2 * abs(x1 - x2) + 2*abs(y1-y2)

type HashMap k v = [(k, v)]

myLookup :: (Eq k) => k -> HashMap k v -> Maybe v
myLookup _ [] = Nothing
myLookup k (x:xs) = if(fst x == k) then Just (snd x) else myLookup k xs
