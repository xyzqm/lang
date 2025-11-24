main = do
  -- read input from main.py
  file <- readFile "main.py"
  print file
  print (1 <= 2)
