data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

myArv :: Arv Int
myArv = (No 3 (No 2 (No 1 Vazia Vazia) Vazia) (No 4 Vazia Vazia))

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No m l r) = m + sumArv l + sumArv r

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar dir ++ [x] ++ listar esq

altura :: Arv a -> Int
altura Vazia = 0
altura (No m l r) = 1 + max (altura l) (altura r)

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No m l r) = [m]
nivel n (No m l r) = nivel (n - 1) l ++ nivel (n - 1) r

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No m l r) = No (f m) (mapArv f l) (mapArv f r)

mais_dir :: Arv a -> a
mais_dir (No m _ Vazia) = m
mais_dir (No m l r) = mais_dir r

ex4_7 :: IO ()
ex4_7 = do
  input <- getLine
  putStrLn $ reverse input

finish :: String -> Bool
finish = foldl (\acc x -> x /= '-' && acc) True

sub :: Char -> String -> String -> String
sub l p1 p2 = zipWith (\x y -> if (y && x == '-') then l else if (x /= '-') then x else '-') p1 [l == y | y <- p2]

advinhoJogo :: String -> String -> Int -> IO ()
advinhoJogo x y count = do
  putStr "?  "
  input <- getChar
  putStrLn ""
  let w = sub input y x
  if w == y
    then do
      putStrLn "Não ocorre"
      advinhoJogo x y (count + 1)
    else
      if finish w
        then do
          putStrLn w
          putStrLn $ "Advinhou em " ++ show (count + 1) ++ " tentativas"
        else do
          putStrLn w
          advinhoJogo x w (count + 1)

advinho :: String -> IO ()
advinho word = advinhoJogo word (replicate (length word) '-') 0

elefantes :: Int -> IO ()
elefantes n = elefantesAux 2 n

elefantesAux :: Int -> Int -> IO ()
elefantesAux i n
  | i < n = do
    putStrLn $ "Se " ++ show i ++ " elefantes incomodam muita gente,"
    putStrLn $ show (i + 1) ++ " elefantes incomodam muito mais!"
    elefantesAux (i + 1) n
  | otherwise = return ()

printStateGame :: Int -> [Int] -> IO ()
printStateGame n l = do
  if length l <= n
    then do return ()
    else do
      putStrLn $ show (n + 1) ++ ": " ++ replicate (l !! n) '*'
      printStateGame (n + 1) l

endGame :: [Int] -> Bool
endGame l = foldl1 (&&) [y == 0 | y <- l]

nim :: Bool -> [Int] -> IO ()
nim turn l = do
  printStateGame 0 l
  if turn
    then do
      putStrLn "Turn Player 1"
    else do
      putStrLn "Turn Player 2"
  putStrLn "Line?"
  lineStr <- getLine
  let line = read lineStr
  putStrLn "Number stars?"
  numStarStr <- getLine
  let numStar = read numStarStr
  if numStar == 0 || line > length l || line < 1 ||numStar < (l !! line - 1) 
    then do
      putStrLn "Invalid move!"
      nim turn l
    else do
      putStrLn "Valid move!"
      let w = take (line - 1) l ++ [l !! (line - 1) - numStar] ++ drop line l
      if endGame w
        then do
          putStrLn "Win!"
          return ()
        else
          if turn
            then nim False w
            else nim True w


