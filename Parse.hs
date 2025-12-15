{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Parse where

import Data.Char
import Lang
import Prelude hiding (any, maybe)

-- parser primitives
data ParseError = ParseError
  { expected :: String,
    found :: String
  }
  deriving (Show)

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
  [] -> ("", Left $ ParseError "any character" "the end of the input")
  (x : xs) -> (xs, Right x)

eof :: Parser ()
eof = Parser $ \case
  [] -> ("", Right ())
  s@(c : _) -> (s, Left $ ParseError "the end of the input" [c])

parseError :: String -> String -> Parser a
parseError expected found = Parser (,Left $ ParseError expected found)

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
maybe p = (Just <$> p) <|> pure Nothing

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

symbol s = string s <* spaces

pToken :: String -> (String -> Bool) -> Parser String
pToken typ predicate = do
  spaces
  token <- many1 (satisfy "not space" (not . isSpace))
  spaces
  if predicate token then return token else parseError typ token

pId = pToken "identifier" isValidId
  where
    isValidId (c : cs) = (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x == '_') cs
    isValidId _ = False

pKwd :: String -> Parser String
pKwd kwd = pToken kwd (== kwd)

run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s

-- our parser
pProgram :: Parser Program
pProgram = many pStatement

pStatement :: Parser Statement
pStatement = choice "statement" [pDisplay, pDefine]

-- pStatement = choice "statement" [pDisplay, pDefine, pWhile, pIf]

pDisplay :: Parser Statement
pDisplay = do
  try $ pKwd "display"
  expr <- pExpr
  id <- maybe (pKwd "read" *> pId)
  return (Display expr id)

pDefine :: Parser Statement
pDefine = do
  try $ pKwd "define"
  id <- pId
  symbol "="
  Define id <$> pExpr

-- parseWhile :: Parser Statement
-- parseWhile = do
--   keyword "while"
--   cond <- parseExpression
--   keyword "run"
--   body <- parseProgram
--   keyword "endwhile"
--   return (While cond body)

-- parseIf :: Parser Statement
-- parseIf = do
--   keyword "if"
--   cond <- parseExpression
--   keyword "then"
--   thenBlock <- parseProgram
--   -- St2 -> endif | else Program endif
--   elseBlock <-
--     (keyword "endif" >> return Nothing)
--       <|> (keyword "else" >> parseProgram >>= \p -> keyword "endif" >> return (Just p))
--   return (If cond thenBlock elseBlock)

-- Expressions
operations = [["<", ">", ">=", "<=", "<>", "="], ["+", "-"], ["*", "/"]]

pLevel :: [[String]] -> Parser Expr
pLevel ops@(cur : nx) = do
  lhs <- pLevel nx
  res <- maybe $ do
    -- try parsing right hand side if it exists
    op <- choice (concat cur) (map symbol cur)
    rhs <- pLevel ops
    return (Binary op lhs rhs)
  case res of
    Just res -> return res
    Nothing -> return lhs
pLevel [] = (Num <$> pInt) <|> (Var <$> pId)

pInt :: Parser Int
pInt = (read <$> many1 digit) <* spaces

pExpr :: Parser Expr
pExpr = pLevel operations

-- -- RelOp -> Add -> Mult -> Neg -> Value
-- parseExpression :: Parser Expr
-- parseExpression = parseAddExpr `chainl1` parseRelOp

-- parseAddExpr :: Parser Expr
-- parseAddExpr = parseMultExpr `chainl1` (parseOp "+" Add <|> parseOp "-" Sub)

-- parseMultExpr :: Parser Expr
-- parseMultExpr = parseNegExpr `chainl1` (parseOp "*" Mul <|> parseOp "/" Div)

-- -- NegExpr -> - Value | Value
-- parseNegExpr :: Parser Expr
-- parseNegExpr =
--   (symbol "-" >> parseValue >>= \v -> return (UnaryOp Negate v))
--     <|> parseValue

-- -- Value -> id | number | ( Expression )
-- parseValue :: Parser Expr
-- parseValue =
--   (Var <$> parseIdentifier)
--     <|> (Num <$> parseNumber)
--     <|> (symbol "(" *> parseExpression <* symbol ")")

-- -- Helpers
-- parseRelOp :: Parser (Expr -> Expr -> Expr)
-- parseRelOp = do
--   BinaryOp . RelOp <$> parseRelationalOperator

-- parseOp :: String -> BinOp -> Parser (Expr -> Expr -> Expr)
-- parseOp sym opConstructor = symbol sym >> return (BinaryOp opConstructor)

-- -- chainl1 handles the Left Recursion: "1 + 2 + 3" becomes ((1+2)+3)
-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = do
--   x <- p
--   rest x
--   where
--     rest x =
--       ( do
--           f <- op
--           y <- p
--           rest (f x y)
--       )
--         <|> return x
