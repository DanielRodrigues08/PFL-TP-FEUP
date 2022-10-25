import Data.Char

cipherLetter :: Char -> Char
cipherLetter l
  | isUpper l = chr (mod (ord l + 13 - ord 'A') 26 + ord 'A')
  | isLower l = chr (mod (ord l + 13 - ord 'a') 26 + ord 'a')
  | otherwise = l

caesar :: String -> String
caesar = map cipherLetter

main :: IO ()
main = do
  input <- getLine
  putStrLn $ caesar input