import Control.Monad
import Interpret
import Parse
import System.Environment

inc :: Int -> IO Int
inc n = do
  putStrLn $ "Incrementing " ++ show n
  return (n + 1)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Executing file: " ++ head args
  -- read from source file
  file <- readFile $ head args

  case run pProgram file of
    Left err -> print err
    Right prog -> do
      _ <- interpret prog
      return ()
