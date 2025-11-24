module Lex (clex) where

import Data.Char (isSpace)
import GHC.Unicode (isAlpha, isDigit)

type Token = (String, Int)

twoCharOps = ["==", "!=", "<=", ">=", "<<", ">>", "&&", "||"]

clex :: String -> Int -> [Token]
-- whitespace
clex (c : cs) ln | c == '\n' = clex cs (ln + 1)
clex (c : cs) ln | isSpace c = clex cs ln
-- numbers
clex (c : cs) ln | isDigit c = (num_token, ln) : clex rest_cs ln
  where
    num_token = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs
-- variables
clex (c : cs) ln | isAlpha c = (var, ln) : clex rest_cs ln
  where
    var = c : takeWhile isAlpha cs
    rest_cs = dropWhile isAlpha cs
-- two char ops
clex (c1 : c2 : cs) ln | [c1, c2] `elem` twoCharOps = ([c1, c2], ln) : clex cs ln
-- comments
clex (c1 : c2 : cs) ln | c1 == '-' && c2 == '-' = clex (dropWhile (/= '\n') cs) ln
-- base cases
clex (c : cs) ln = ([c], ln) : clex cs ln
clex [] ln = []
