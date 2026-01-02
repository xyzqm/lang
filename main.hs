-- import Lex (clex)

import Interpret
import Parse

main :: IO ()
main = do
  -- read from source file
  file <- readFile "src.simple"

  case run pProgram file of
    Left err -> print err
    Right prog -> do
      _ <- execProgram (Env mempty) prog
      return ()
