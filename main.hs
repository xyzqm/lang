import Interpret
import Parse

main :: IO ()
main = do
  -- read from source file
  file <- readFile "max.simple"

  case run pProgram file of
    Left err -> print err
    Right prog -> do
      _ <- interpret prog
      return ()
