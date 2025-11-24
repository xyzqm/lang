import Lex (clex)

main :: IO ()
main = do
  -- read input from main.py
  file <- readFile "src.hs"
  print (clex file 1)
  print (lex file)
