module Interpret (interpret) where

import Data.Map (Map)
import Data.Map qualified as Map
import Lang

newtype Env = Env {envVars :: Map String Int}

instance Show Env where
  show (Env env) = "Env {envVars = " ++ show env ++ "}"

instance Eq Env where
  (Env env1) == (Env env2) = env1 == env2

-- | Evaluate an expression in the given environment
evalExpr :: Env -> Expr -> Int
evalExpr (Env env) expr = case expr of
  Num n -> n
  Var x -> Map.findWithDefault 0 x env
  Binary op e1 e2 ->
    let v1 = evalExpr (Env env) e1
        v2 = evalExpr (Env env) e2
     in case op of
          "+" -> v1 + v2
          "-" -> v1 - v2
          "*" -> v1 * v2
          "/" -> if v2 == 0 then 0 else v1 `div` v2
          "=" -> if v1 == v2 then 1 else 0
          "<>" -> if v1 /= v2 then 1 else 0
          "<" -> if v1 < v2 then 1 else 0
          "<=" -> if v1 <= v2 then 1 else 0
          ">" -> if v1 > v2 then 1 else 0
          ">=" -> if v1 >= v2 then 1 else 0
          _ -> 0

-- | Execute a statement, possibly printing output, and return the new environment
execStatement :: Env -> Statement -> IO Env
execStatement env stmt = case stmt of
  Display expr readId -> do
    let val = evalExpr env expr
    print val
    case readId of
      Nothing -> return env
      Just name -> do
        putStr (name ++ ": ")
        input <- getLine
        case reads input of
          [(n, "")] ->
            let Env vars = env
             in return $ Env (Map.insert name n vars)
          _ -> ioError (userError "Invalid input, expected an integer.")
  Define name expr ->
    let val = evalExpr env expr
        Env vars = env
     in return $ Env (Map.insert name val vars)
  While cond body -> loop env
    where
      loop e =
        if evalExpr e cond /= 0
          then execProgram e body >>= loop
          else return e
  If cond thenProg elseProg ->
    if evalExpr env cond /= 0
      then execProgram env thenProg
      else case elseProg of
        Nothing -> return env
        Just prog -> execProgram env prog
  Void -> return env

-- | Execute a program (list of statements)
execProgram :: Env -> Program -> IO Env
execProgram env [] = return env
execProgram env (s : ss) = do
  env' <- execStatement env s
  execProgram env' ss

interpret :: Program -> IO Env
interpret = execProgram (Env mempty)
