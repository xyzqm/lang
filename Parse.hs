{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Parse where

import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ord qualified
import Debug.Trace
import Lang
import Prelude hiding (any, maybe)

-- parser primitives
data ParseError = ParseError
  { expected :: String,
    location :: String
  }

instance Show ParseError where
  show (ParseError expct loc) =
    "ParseError: expected "
      ++ show expct
      ++ " at\n"
      ++ take 40 loc
      ++ (if length loc > 40 then "..." else "")

newtype Parser a = Parser {runParser :: String -> (String, Either ParseError a)}
  deriving (Functor)

instance Applicative Parser where
  pure c = Parser (,Right c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Right f) -> fmap f <$> runParser pa s'
    (s', Left e) -> (s', Left e)

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    (s', Right a) -> runParser (f a) s'
    (s', Left e) -> (s', Left e)

any :: Parser Char
any = Parser $ \case
  [] -> ("", Left $ ParseError "any character" "")
  (x : xs) -> (xs, Right x)

eof :: Parser ()
eof = Parser $ \case
  [] -> ("", Right ())
  s@(c : _) -> (s, Left $ ParseError "the end of the input" s)

parseError :: String -> String -> Parser a
parseError expected found = Parser $ \s -> (s, Left $ ParseError expected s)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else parseError description [c]

-- when we encounter an error, backtrack instead of failing
try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  (_s', Left err) -> (s, Left err)
  success -> success

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

maybe :: Parser a -> Parser (Maybe a)
maybe p = (Just <$> try p) <|> pure Nothing

-- p1 <|> (p2 <|> (p3 <|> (... <|> noMatch)))
choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) noMatch
  where
    noMatch = parseError description "no match"

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> return []
many1 p = do
  first <- p
  rest <- many p
  return (first : rest)

char c = satisfy [c] (== c)

space = satisfy "space" isSpace

digit = satisfy "digit" isDigit

spaces = many space

string = traverse char

symbol s = try (string s <* spaces) <|> parseError s ""

pId :: Parser String
pId = do
  c <- satisfy "identifier start" (\x -> isAlpha x || x == '_')
  cs <- many (satisfy "identifier" (\x -> isAlphaNum x || x == '_'))
  spaces
  return (c : cs)

run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s

-- STATEMENTS.
pProgram :: Parser Program
-- remove whitespace from the start
pProgram = spaces *> many pStatement

pStatement :: Parser Statement
pStatement = choice "statement" [pDisplay, pDefine, pWhile, pIf]

pDisplay :: Parser Statement
pDisplay = do
  try $ symbol "display"
  expr <- pExpr
  id <- maybe (symbol "read" *> pId)
  return (Display expr id)

pDefine :: Parser Statement
pDefine = do
  try $ symbol "define"
  id <- pId
  symbol "="
  Define id <$> pExpr

pWhile :: Parser Statement
pWhile = do
  try $ symbol "while"
  cond <- pExpr
  symbol "run"
  body <- pProgram
  symbol "endwhile"
  return (While cond body)

pIf :: Parser Statement
pIf = do
  try $ symbol "if"
  cond <- pExpr
  symbol "then"
  thn <- pProgram
  -- try parsing else
  els <- maybe $ do
    symbol "else"
    pProgram
  symbol "endif"
  return (If cond thn els)

-- EXPRESSIONS.
operations = [["<", ">", ">=", "<=", "<>", "="], ["+", "-"], ["*", "/"]]

pLevel :: [[String]] -> Parser Expr
pLevel ops@(cur : nx) = do
  lhs <- pLevel nx
  let sortedOps = sortBy (comparing (Data.Ord.Down . length)) cur -- sort in desending order of length
  res <- maybe $ do
    op <- choice (concat sortedOps) (map (try . symbol) sortedOps)
    rhs <- pLevel ops
    return (Binary op lhs rhs)
  case res of
    Just res -> return res
    Nothing -> return lhs
pLevel [] =
  choice
    "factor"
    [ Num <$> pInt,
      Var <$> pId,
      do
        symbol "("
        e <- pExpr
        symbol ")"
        return e,
      do
        symbol "-"
        Binary "-" (Num 0) <$> pExpr
    ]

pInt :: Parser Int
pInt = (read <$> many1 digit) <* spaces

pExpr :: Parser Expr
pExpr = pLevel operations
