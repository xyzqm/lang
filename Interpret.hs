-- | The 'Interpret' module provides evaluation and execution for the language AST.
module Interpret (interpret) where

import Data.Map (Map)
import Data.Map qualified as Map
import Lang

-- | The environment mapping variable names to integer values.
newtype Env = Env {envVars :: Map String Int}

instance Show Env where
  -- \| Pretty-prints an environment.
  show (Env env) = "Env {envVars = " ++ show env ++ "}"

instance Eq Env where
  -- \| Checks equality of two environments.
  (Env env1) == (Env env2) = env1 == env2

-- | Evaluates an expression in the given environment.
evalExpr :: Env -> Expr -> Int
evalExpr (Env env) expr = case expr of
  Num n -> n
  Var x -> Map.findWithDefault 0 x env
  Binary op e1 e2 ->
    let v1 = evalExpr (Env env) e1
        v2 = evalExpr (Env env) e2
     in case op of
          "or" -> max v1 v2
          "and" -> min v1 v2
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

-- | Executes a statement, possibly printing output, and returns the new environment.
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
      -- \| Helper function to repeatedly execute the body while the condition is true.
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

-- | Executes a program (list of statements) in the given environment.
execProgram :: Env -> Program -> IO Env
execProgram env [] = return env
execProgram env (s : ss) = do
  env' <- execStatement env s
  execProgram env' ss

-- | Interprets a program from scratch (with an empty environment).
interpret :: Program -> IO Env
interpret = execProgram (Env mempty)
