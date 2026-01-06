{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Parse (run, pProgram) where

import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ord qualified
import Debug.Trace
import Lang
import Prelude hiding (any, maybe)

-- | Represents a parsing error, including what was expected and the location in the input.
data ParseError = ParseError
  { expected :: String,
    location :: String
  }

instance Show ParseError where
  -- \| Pretty-prints a 'ParseError'.
  show (ParseError expct loc) =
    "ParseError: expected "
      ++ show expct
      ++ " at\n"
      ++ take 40 loc
      ++ (if length loc > 40 then "..." else "")

-- | The parser type. Wraps a function from input string to remaining string and either a parse error or a result.
newtype Parser a = Parser {runParser :: String -> (String, Either ParseError a)}
  deriving (Functor)

instance Applicative Parser where
  -- \| Lifts a value into the parser.
  pure c = Parser (,Right c)

  -- \| Applies a parsed function to a parsed value.
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Right f) -> fmap f <$> runParser pa s'
    (s', Left e) -> (s', Left e)

instance Monad Parser where
  -- \| Monadic bind for parsers.
  pa >>= f = Parser $ \s -> case runParser pa s of
    (s', Right a) -> runParser (f a) s'
    (s', Left e) -> (s', Left e)

-- | Parses any single character.
any :: Parser Char
any = Parser $ \case
  [] -> ("", Left $ ParseError "any character" "")
  (x : xs) -> (xs, Right x)

-- | Succeeds only at the end of input.
eof :: Parser ()
eof = Parser $ \case
  [] -> ("", Right ())
  s@(c : _) -> (s, Left $ ParseError "the end of the input" s)

-- | Always fails with the given expected string.
parseError :: String -> Parser a
parseError expected = Parser $ \s -> (s, Left $ ParseError expected s)

-- | Parses a character satisfying a predicate, or fails with a description.
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else parseError description

-- when we encounter an error, backtrack instead of failing

-- | Tries a parser, backtracking on failure.
try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  (_s', Left err) -> (s, Left err)
  success -> success

-- | Choice combinator: tries the first parser, and if it fails without consuming input, tries the second.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

-- | Optionally parses a value, returning 'Nothing' if parsing fails.
maybe :: Parser a -> Parser (Maybe a)
maybe p = (Just <$> try p) <|> pure Nothing

-- p1 <|> (p2 <|> (p3 <|> (... <|> noMatch)))

-- | Tries each parser in the list in order, failing if none succeed.
choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) noMatch
  where
    noMatch = parseError description

-- | Parses zero or more occurrences of the given parser.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | Parses one or more occurrences of the given parser.
many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest <- many p
  return (first : rest)

-- | Parses a specific character.
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- | Parses a whitespace character.
space :: Parser Char
space = satisfy "space" isSpace

-- | Parses a digit character.
digit :: Parser Char
digit = satisfy "digit" isDigit

-- | Parses zero or more whitespace characters.
spaces :: Parser String
spaces = many space

-- | Parses a specific string.
string :: String -> Parser String
string = traverse char

-- | Parses a symbol (string) followed by optional spaces.
symbol :: String -> Parser String
symbol s = try (string s <* spaces) <|> parseError s

-- | Parses an identifier (variable name).
pId :: Parser String
pId = do
  c <- satisfy "identifier start" (\x -> isAlpha x || x == '_')
  cs <- many (satisfy "identifier" (\x -> isAlphaNum x || x == '_'))
  spaces
  return (c : cs)

-- | Runs a parser on a string, requiring that the entire input is consumed.
run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s

-- STATEMENTS.

-- | Parses a program (a sequence of statements), skipping leading whitespace.
pProgram :: Parser Program
pProgram = spaces *> many pStatement

-- | Parses a single statement.
pStatement :: Parser Statement
pStatement = choice "statement" [pDisplay, pDefine, pWhile, pIf, pComment]

-- | Parses a comment statement.
pComment :: Parser Statement
pComment = do
  try $ symbol "#"
  comment <- many (satisfy "comment" (/= '\n'))
  symbol "\n"
  return Void

-- | Parses a display statement, optionally with a read identifier.
pDisplay :: Parser Statement
pDisplay = do
  try $ symbol "display"
  expr <- pExpr
  id <- maybe (symbol "read" *> pId)
  return (Display expr id)

-- | Parses a define statement.
pDefine :: Parser Statement
pDefine = do
  try $ symbol "define"
  id <- pId
  symbol "="
  Define id <$> pExpr

-- | Parses a while loop statement.
pWhile :: Parser Statement
pWhile = do
  try $ symbol "while"
  cond <- pExpr
  symbol "run"
  body <- pProgram
  symbol "endwhile"
  return (While cond body)

-- | Parses an if statement, with optional else branch.
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

-- | Operator precedence table for expressions.
operations :: [[String]]
operations = [["and", "or"], ["<", ">", ">=", "<=", "<>", "="], ["+", "-"], ["*", "/"]]

-- | Parses an expression at a given precedence level, left-associative.
pLevel :: [[String]] -> Parser Expr
pLevel (cur : nx) = do
  lhs <- pLevel nx
  let pNx = do
        -- sort in descending order of length
        let sortedOps = sortBy (comparing (Data.Ord.Down . length)) cur
        op <- choice (concat sortedOps) (map (try . symbol) sortedOps)
        expr <- pLevel nx
        return (op, expr)
  rem <- many pNx
  return $ foldl (\acc (op, expr) -> Binary op acc expr) lhs rem
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

-- | Parses an integer literal.
pInt :: Parser Int
pInt = (read <$> many1 digit) <* spaces

-- | Parses an expression, starting at the top precedence level.
pExpr :: Parser Expr
pExpr = pLevel operations
