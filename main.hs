import Control.Monad
import Interpret
import Parse

inc :: Int -> IO Int
inc n = do
  putStrLn $ "Incrementing " ++ show n
  return (n + 1)

main :: IO ()
main = do
  -- read from source file
  file <- readFile "binary.simple"

  case run pProgram file of
    Left err -> print err
    Right prog -> do
      _ <- interpret prog
      return ()
