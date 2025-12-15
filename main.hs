-- import Lex (clex)
import Parse

main :: IO ()
main = do
  -- read from source file
  file <- readFile "src.simple"
  print $ run pProgram file
