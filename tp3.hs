mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

dec2int :: [Int] -> Int
dec2int l = foldl (\x y -> x * 10 + y) 0 l

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

maximum' :: Ord a => [a] -> a
maximum' l = foldl1 max l

maximum'' :: Ord a => [a] -> a
maximum'' l = foldr1 max l

minimum' :: Ord a => [a] -> a
minimum' l = foldl1 min l

minimum'' :: Ord a => [a] -> a
minimum'' l = foldr1 min l

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f l = foldl f (head l) (tail l)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f l = foldr f (last l) (init l)

mdc :: Int -> Int -> Int
mdc a b = fst (until (\(a, b) -> b == 0) (\(a, b) -> (b, mod a b)) (a, b))

(+++) :: [a] -> [a] -> [a]
(+++) l1 l2 = foldr (:) l2 l1

concat' :: [[a]] -> [a]
concat' l1 = foldr (++) [] l1

reverse' :: [a] -> [a]
reverse' l1 = foldr (\x xs -> xs ++ [x]) [] l1

reverse'' :: [a] -> [a]
reverse'' l1 = foldl (\xs x -> x : xs) [] l1

elem' :: Eq a => a -> [a] -> Bool
elem' x l = any (\y -> x == y) l

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ i [] = [i]
scanl' f i (x : xs) = i : scanl' f (f i x) xs
